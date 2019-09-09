USE [master]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF OBJECT_ID('[dbo].[AddDatabaseOnSecondary]') IS NOT NULL DROP PROCEDURE [dbo].[AddDatabaseOnSecondary]
GO
CREATE PROCEDURE [dbo].[AddDatabaseOnSecondary]

/* 
Purpose: This procedure can be used for regular restores of database that is part of availability group. this procedure is called on secondary
replica and adding given database to availability groupin folowwing steps:
 - restore given full backup
 - restore given backup of transaction log
 - join database to availability group on secondary

Author:	Tomas Rybnicky
Date of last update: 
	v1.2 - 09.09.2019 - added possiblity to set autogrowth for restored database based on model database settings (RestoreDatabase stored procedure)

List of previous revisions:
	v1.0 - 01.11.2018 - stored procedures cleaned and tested. Solution is usable now.
	v0.1 - 31.10.2018 - Initial solution containing all not necesary scripting from testing and development work
	
Execution example:					
	EXEC [master].[dbo].[AddDatabaseOnSecondary]
	@FullBackupFile = N'\\Path\To\BackupFile\FullBackup.bak',
	@TlogBackupFile = N'\\Path\To\BackupFile\TlogBackup.trn',
	@Database = N'TestDB',
	@AvailabilityGroup = N'AvailabilityGroupName',
	@LogToTable = 'Y'						
*/
@FullBackupFile		NVARCHAR(1024),			-- Database backup file taken on primary replica
@TlogBackupFile		NVARCHAR(1024),			-- Transaction log backup file taken on primary replica
@Database			SYSNAME,				-- Name of  database
@AvailabilityGroup	SYSNAME,				-- Name of Availability Group that is to be used for database
@LogToTable			CHAR(1) = 'N'			-- Flag if restore commands are to be tracked in CommandLog table

AS

BEGIN
	
	SET NOCOUNT ON
	----------------------------------------------------------------------------------------
	-- declare variables used in script
	----------------------------------------------------------------------------------------
	DECLARE @ErrorMessage			NVARCHAR(MAX)
	DECLARE @InstanceDataPath		VARCHAR(1024)
	DECLARE @InstanceTlogPath		VARCHAR(1024)
	DECLARE @Version				NUMERIC(18,10)
	DECLARE @Tsql					VARCHAR(MAX)
	DECLARE @Msg					VARCHAR(MAX)

	SET @Msg = @@SERVERNAME + ' : Add database ' + @Database + ' to Availability Group ' + @AvailabilityGroup
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	----------------------------------------------------------------------------------------
	-- check requirements
	----------------------------------------------------------------------------------------	
	SET @Msg = CHAR(13) + CHAR(10) + 'STEP (' + @@SERVERNAME + '): Checking requirements'
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	SET @Msg = ' - permissions'
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;
	IF IS_SRVROLEMEMBER('sysadmin') = 0
	BEGIN	
		SET @ErrorMessage = 'You need to be a member of the sysadmin server role to run this procedure.'
		GOTO QuitWithRollback
	END

	SET @Msg = ' - procedure CommandExecute'
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;	
	IF NOT EXISTS (SELECT * FROM sys.objects objects INNER JOIN sys.schemas schemas ON objects.[schema_id] = schemas.[schema_id] WHERE objects.[type] = 'P' AND schemas.[name] = 'dbo' AND objects.[name] = 'CommandExecute')
	BEGIN
		SET @ErrorMessage = 'The stored procedure CommandExecute is missing. Download https://ola.hallengren.com/scripts/CommandExecute.sql.' + CHAR(13) + CHAR(10) + ' '
		GOTO QuitWithRollback
	END
	
	SET @Msg = ' - table CommandLog'
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;	
	IF @LogToTable = 'Y' AND NOT EXISTS (SELECT * FROM sys.objects objects INNER JOIN sys.schemas schemas ON objects.[schema_id] = schemas.[schema_id] WHERE objects.[type] = 'U' AND schemas.[name] = 'dbo' AND objects.[name] = 'CommandLog')
	BEGIN
		SET @ErrorMessage = 'The table CommandLog is missing. Download https://ola.hallengren.com/scripts/CommandLog.sql.' + CHAR(13) + CHAR(10) + ' '
		GOTO QuitWithRollback
	END

	----------------------------------------------------------------------------------------
	-- check availability group
	----------------------------------------------------------------------------------------
	IF @AvailabilityGroup IS NOT NULL
	BEGIN	
		SET @Msg = ' - availability group'
		RAISERROR(@Msg, 0, 1) WITH NOWAIT;

		-- check if HADR enabled
		IF (SELECT SERVERPROPERTY ('IsHadrEnabled')) <> 1 GOTO HadrNotEnabled

		-- check given AG name
		IF NOT EXISTS (SELECT name FROM master.sys.availability_groups WHERE name = @AvailabilityGroup) GOTO UnknownAvailabilityGroup

		-- check if secondary
		IF NOT EXISTS (SELECT ar.replica_server_name
		FROM master.sys.dm_hadr_availability_group_states hags
			INNER JOIN master.sys.availability_replicas ar ON ar.group_id = hags.group_id
			INNER JOIN master.sys.availability_groups ag ON ag.group_id = hags.group_id
		WHERE
			ag.name = @AvailabilityGroup
			AND ar.replica_server_name NOT LIKE hags.primary_replica
			AND ar.replica_server_name = @@SERVERNAME) GOTO NotSecondaryReplica
	END

	----------------------------------------------------------------------------------------
	-- restore database
	----------------------------------------------------------------------------------------
	SET @Msg = CHAR(13) + CHAR(10) + 'STEP (' + @@SERVERNAME + '): Restoring database ' + @Database
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	SET @Msg = CHAR(13) + CHAR(10) + ' - full backup ' + @FullBackupFile
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	SET @Tsql = N'RESTORE DATABASE ' + @Database + ' FROM DISK = N''' + @FullBackupFile + ''' WITH FILE=1, REPLACE, NORECOVERY'

	EXEC [master].[dbo].[CommandExecute]
		@Command = @Tsql,
		@CommandType = 'RESTORE_DATABASE',
		@DatabaseName = @Database,
		@Mode = 1,
		@LogToTable = @LogToTable,
		@Execute = 'Y'

	----------------------------------------------------------------------------------------
	-- restore transaction log
	----------------------------------------------------------------------------------------
	SET @Msg = CHAR(13) + CHAR(10) + ' - restore of transaction log ' + @TlogBackupFile
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	SET @Tsql = N'RESTORE LOG ' + @Database + ' FROM DISK = N''' + @TlogBackupFile + ''' WITH FILE=1, REPLACE, NORECOVERY'

	EXEC [master].[dbo].[CommandExecute]
		@Command = @Tsql,
		@CommandType = 'RESTORE_LOG',
		@DatabaseName = @Database,
		@Mode = 1,
		@LogToTable = @LogToTable,
		@Execute = 'Y'

	----------------------------------------------------------------------------------------
	-- add database to availability group on secondary replica
	----------------------------------------------------------------------------------------
	IF @AvailabilityGroup IS NOT NULL
	BEGIN
		SET @Msg = CHAR(13) + CHAR(10) + 'STEP (' + @@SERVERNAME + '): Add database ' + @Database + ' to Availability Group ' + @AvailabilityGroup
		RAISERROR(@Msg, 0, 1) WITH NOWAIT;

		SET @Msg = ' - add on secondary replica ' + @@SERVERNAME
		RAISERROR(@Msg, 0, 1) WITH NOWAIT;

		SET @Tsql = 'ALTER DATABASE [' + @Database + '] SET HADR AVAILABILITY GROUP = [' + @AvailabilityGroup + '];'

		EXEC [master].[dbo].[CommandExecute]
		@Command = @Tsql,
		@CommandType = 'AG_JOIN_SECONDARY',
		@DatabaseName = @Database,
		@Mode = 2,
		@LogToTable = @LogToTable,
		@Execute = 'Y'
	END
	
	----------------------------------------------------------------------------------------
	-- finish
	----------------------------------------------------------------------------------------
	GOTO Finish

	----------------------------------------------------------------------------------------
	-- skip restore because HADR is not enabled on instance
	----------------------------------------------------------------------------------------
	HadrNotEnabled:
		SET @ErrorMessage = 'HADR not enabled on instance ' + @@SERVERNAME + '. Are you targeting right instance? Please check and rerun. Exitting...'
		GOTO QuitWithRollback

	----------------------------------------------------------------------------------------
	-- skip restore because wrong Availabilit Group name given
	----------------------------------------------------------------------------------------
	UnknownAvailabilityGroup:
		SET @ErrorMessage = 'Availability group ' + @AvailabilityGroup + ' not found! Check input parameters and try again. Exitting...'
		GOTO QuitWithRollback

	----------------------------------------------------------------------------------------
	-- skip restore because this server is not secondary replica
	----------------------------------------------------------------------------------------
	NotSecondaryReplica:
		SET @ErrorMessage = 'Server ' + @@SERVERNAME + ' is not secondary replica of Availability Group ' + @AvailabilityGroup + '! Use this procedure on secondary replicas only. Exitting...'
		GOTO QuitWithRollback

	----------------------------------------------------------------------------------------
	-- handle error message
	----------------------------------------------------------------------------------------
	QuitWithRollback:
		IF (@@TRANCOUNT > 0) ROLLBACK TRANSACTION
		RAISERROR(@ErrorMessage, 16, 1)
		GOTO EndOfFile

	----------------------------------------------------------------------------------------
	-- just finishing script
	----------------------------------------------------------------------------------------
	Finish:
		SET @Msg = 'Database ' + @Database + ' joined Availability Group ' + @AvailabilityGroup + ' secondary ' + @@SERVERNAME
		RAISERROR(@Msg, 0, 1) WITH NOWAIT;
	
	----------------------------------------------------------------------------------------
	-- put any cleanup stuff here as script will always hit this part
	----------------------------------------------------------------------------------------
	EndOfFile:
END