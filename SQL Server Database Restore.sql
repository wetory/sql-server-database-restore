/*  
Title: SQL Server Database Restore - deployment script
Description: Script is creating standardized restore procedures that can be used for regular restored of database, for example from PROD to DEV environment. Highly tied with
from Olla Hallengreen's maintenance solution https://ola.hallengren.com/ as it is using its CommandLog table and CommandExecute stored procedure. So can and should co-exist 
together with this maintenance on same instance :)

It is creating following stuff in SQL Server instance:
	- stored procedures in master database

Author: Tomas Rybnicky 
Date of last update: 
	v1.2 - 09.09.2019 - added possiblity to set autogrowth for restored database based on model database settings (RestoreDatabase stored procedure)

List of previous revisions:
	v1.0 - 01.11.2018 - stored procedures cleaned and tested. Solution is usable now.
	v0.1 - 31.10.2018 - Initial solution containing all not necesary scripting from testing and development work
*/
USE [master]
GO
SET NOCOUNT ON
GO

-- declare variables used in script
DECLARE @Version	NUMERIC(18,10)

PRINT 'SQL Server Database Restore - deployment of solution'
PRINT '-------------------------------------------------------------------------'
	
----------------------------------------------------------------------------------------
-- checking core requirements
----------------------------------------------------------------------------------------
IF IS_SRVROLEMEMBER('sysadmin') = 0
BEGIN
	RAISERROR('You need to be a member of the sysadmin server role to install the solution.',16,1)
END

SET @Version = CAST(LEFT(CAST(SERVERPROPERTY('ProductVersion') AS nvarchar(max)),CHARINDEX('.',CAST(SERVERPROPERTY('ProductVersion') AS nvarchar(max))) - 1) + '.' + REPLACE(RIGHT(CAST(SERVERPROPERTY('ProductVersion') AS nvarchar(max)), LEN(CAST(SERVERPROPERTY('ProductVersion') AS nvarchar(max))) - CHARINDEX('.',CAST(SERVERPROPERTY('ProductVersion') AS nvarchar(max)))),'.','') AS numeric(18,10))
IF @Version < 10 PRINT 'WARNING : You are running pretty old nad not supprted version of SQL Server, using this script on your own risk'


----------------------------------------------------------------------------------------
-- creating Ola Halengreen's stuff - https://ola.hallengren.com/
----------------------------------------------------------------------------------------

-- table CommandLog - https://ola.hallengren.com/scripts/CommandLog.sql
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[CommandLog]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[CommandLog](
	[ID] [int] IDENTITY(1,1) NOT NULL,
	[DatabaseName] [sysname] NULL,
	[SchemaName] [sysname] NULL,
	[ObjectName] [sysname] NULL,
	[ObjectType] [char](2) NULL,
	[IndexName] [sysname] NULL,
	[IndexType] [tinyint] NULL,
	[StatisticsName] [sysname] NULL,
	[PartitionNumber] [int] NULL,
	[ExtendedInfo] [xml] NULL,
	[Command] [nvarchar](max) NOT NULL,
	[CommandType] [nvarchar](60) NOT NULL,
	[StartTime] [datetime] NOT NULL,
	[EndTime] [datetime] NULL,
	[ErrorNumber] [int] NULL,
	[ErrorMessage] [nvarchar](max) NULL,
 CONSTRAINT [PK_CommandLog] PRIMARY KEY CLUSTERED
(
	[ID] ASC
) WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
)
END
GO
PRINT 'STEP : Created table [dbo].[CommandLog] in master database.'
GO
-- stored procedure CommandExecute - https://ola.hallengren.com/scripts/CommandExecute.sql
IF OBJECT_ID('[dbo].[CommandExecute]', 'P') IS NOT NULL
    DROP PROCEDURE [dbo].[CommandExecute];
GO
CREATE PROCEDURE [dbo].[CommandExecute]
@Command nvarchar(max),
@CommandType nvarchar(max),
@Mode int,
@Comment nvarchar(max) = NULL,
@DatabaseName nvarchar(max) = NULL,
@SchemaName nvarchar(max) = NULL,
@ObjectName nvarchar(max) = NULL,
@ObjectType nvarchar(max) = NULL,
@IndexName nvarchar(max) = NULL,
@IndexType int = NULL,
@StatisticsName nvarchar(max) = NULL,
@PartitionNumber int = NULL,
@ExtendedInfo xml = NULL,
@LockMessageSeverity int = 16,
@LogToTable nvarchar(max),
@Execute nvarchar(max)

AS

BEGIN

  ----------------------------------------------------------------------------------------------------
  --// Source:  https://ola.hallengren.com                                                        //--
  --// License: https://ola.hallengren.com/license.html                                           //--
  --// GitHub:  https://github.com/olahallengren/sql-server-maintenance-solution                  //--
  --// Version: 2018-10-28 14:45:02                                                               //--
  ----------------------------------------------------------------------------------------------------

  SET NOCOUNT ON

  DECLARE @StartMessage nvarchar(max)
  DECLARE @EndMessage nvarchar(max)
  DECLARE @ErrorMessage nvarchar(max)
  DECLARE @ErrorMessageOriginal nvarchar(max)
  DECLARE @Severity int

  DECLARE @StartTime datetime
  DECLARE @EndTime datetime

  DECLARE @StartTimeSec datetime
  DECLARE @EndTimeSec datetime

  DECLARE @ID int

  DECLARE @Error int
  DECLARE @ReturnCode int

  SET @Error = 0
  SET @ReturnCode = 0

  ----------------------------------------------------------------------------------------------------
  --// Check core requirements                                                                    //--
  ----------------------------------------------------------------------------------------------------

  IF NOT (SELECT [compatibility_level] FROM sys.databases WHERE database_id = DB_ID()) >= 90
  BEGIN
    SET @ErrorMessage = 'The database ' + QUOTENAME(DB_NAME(DB_ID())) + ' has to be in compatibility level 90 or higher.' + CHAR(13) + CHAR(10) + ' '
    RAISERROR(@ErrorMessage,16,1) WITH NOWAIT
    SET @Error = @@ERROR
  END

  IF NOT (SELECT uses_ansi_nulls FROM sys.sql_modules WHERE [object_id] = @@PROCID) = 1
  BEGIN
    SET @ErrorMessage = 'ANSI_NULLS has to be set to ON for the stored procedure.' + CHAR(13) + CHAR(10) + ' '
    RAISERROR(@ErrorMessage,16,1) WITH NOWAIT
    SET @Error = @@ERROR
  END

  IF NOT (SELECT uses_quoted_identifier FROM sys.sql_modules WHERE [object_id] = @@PROCID) = 1
  BEGIN
    SET @ErrorMessage = 'QUOTED_IDENTIFIER has to be set to ON for the stored procedure.' + CHAR(13) + CHAR(10) + ' '
    RAISERROR(@ErrorMessage,16,1) WITH NOWAIT
    SET @Error = @@ERROR
  END

  IF @LogToTable = 'Y' AND NOT EXISTS (SELECT * FROM sys.objects objects INNER JOIN sys.schemas schemas ON objects.[schema_id] = schemas.[schema_id] WHERE objects.[type] = 'U' AND schemas.[name] = 'dbo' AND objects.[name] = 'CommandLog')
  BEGIN
    SET @ErrorMessage = 'The table CommandLog is missing. Download https://ola.hallengren.com/scripts/CommandLog.sql.' + CHAR(13) + CHAR(10) + ' '
    RAISERROR(@ErrorMessage,16,1) WITH NOWAIT
    SET @Error = @@ERROR
  END

  IF @Error <> 0
  BEGIN
    SET @ReturnCode = @Error
    GOTO ReturnCode
  END

  ----------------------------------------------------------------------------------------------------
  --// Check input parameters                                                                     //--
  ----------------------------------------------------------------------------------------------------

  IF @Command IS NULL OR @Command = ''
  BEGIN
    SET @ErrorMessage = 'The value for the parameter @Command is not supported.' + CHAR(13) + CHAR(10) + ' '
    RAISERROR(@ErrorMessage,16,1) WITH NOWAIT
    SET @Error = @@ERROR
  END

  IF @CommandType IS NULL OR @CommandType = '' OR LEN(@CommandType) > 60
  BEGIN
    SET @ErrorMessage = 'The value for the parameter @CommandType is not supported.' + CHAR(13) + CHAR(10) + ' '
    RAISERROR(@ErrorMessage,16,1) WITH NOWAIT
    SET @Error = @@ERROR
  END

  IF @Mode NOT IN(1,2) OR @Mode IS NULL
  BEGIN
    SET @ErrorMessage = 'The value for the parameter @Mode is not supported.' + CHAR(13) + CHAR(10) + ' '
    RAISERROR(@ErrorMessage,16,1) WITH NOWAIT
    SET @Error = @@ERROR
  END

  IF @LockMessageSeverity NOT IN(10,16) OR @LockMessageSeverity IS NULL
  BEGIN
    SET @ErrorMessage = 'The value for the parameter @LockMessageSeverity is not supported.' + CHAR(13) + CHAR(10) + ' '
    RAISERROR(@ErrorMessage,16,1) WITH NOWAIT
    SET @Error = @@ERROR
  END

  IF @LogToTable NOT IN('Y','N') OR @LogToTable IS NULL
  BEGIN
    SET @ErrorMessage = 'The value for the parameter @LogToTable is not supported.' + CHAR(13) + CHAR(10) + ' '
    RAISERROR(@ErrorMessage,16,1) WITH NOWAIT
    SET @Error = @@ERROR
  END

  IF @Execute NOT IN('Y','N') OR @Execute IS NULL
  BEGIN
    SET @ErrorMessage = 'The value for the parameter @Execute is not supported.' + CHAR(13) + CHAR(10) + ' '
    RAISERROR(@ErrorMessage,16,1) WITH NOWAIT
    SET @Error = @@ERROR
  END

  IF @Error <> 0
  BEGIN
    SET @ReturnCode = @Error
    GOTO ReturnCode
  END

  ----------------------------------------------------------------------------------------------------
  --// Log initial information                                                                    //--
  ----------------------------------------------------------------------------------------------------

  SET @StartTime = GETDATE()
  SET @StartTimeSec = CONVERT(datetime,CONVERT(nvarchar,@StartTime,120),120)

  SET @StartMessage = 'Date and time: ' + CONVERT(nvarchar,@StartTimeSec,120)
  RAISERROR(@StartMessage,10,1) WITH NOWAIT

  SET @StartMessage = 'Command: ' + @Command
  SET @StartMessage = REPLACE(@StartMessage,'%','%%')
  RAISERROR(@StartMessage,10,1) WITH NOWAIT

  IF @Comment IS NOT NULL
  BEGIN
    SET @StartMessage = 'Comment: ' + @Comment
    SET @StartMessage = REPLACE(@StartMessage,'%','%%')
    RAISERROR(@StartMessage,10,1) WITH NOWAIT
  END

  IF @LogToTable = 'Y'
  BEGIN
    INSERT INTO dbo.CommandLog (DatabaseName, SchemaName, ObjectName, ObjectType, IndexName, IndexType, StatisticsName, PartitionNumber, ExtendedInfo, CommandType, Command, StartTime)
    VALUES (@DatabaseName, @SchemaName, @ObjectName, @ObjectType, @IndexName, @IndexType, @StatisticsName, @PartitionNumber, @ExtendedInfo, @CommandType, @Command, @StartTime)
  END

  SET @ID = SCOPE_IDENTITY()

  ----------------------------------------------------------------------------------------------------
  --// Execute command                                                                            //--
  ----------------------------------------------------------------------------------------------------

  IF @Mode = 1 AND @Execute = 'Y'
  BEGIN
    EXECUTE(@Command)
    SET @Error = @@ERROR
    SET @ReturnCode = @Error
  END

  IF @Mode = 2 AND @Execute = 'Y'
  BEGIN
    BEGIN TRY
      EXECUTE(@Command)
    END TRY
    BEGIN CATCH
      SET @Error = ERROR_NUMBER()
      SET @ErrorMessageOriginal = ERROR_MESSAGE()

      SET @ErrorMessage = 'Msg ' + CAST(ERROR_NUMBER() AS nvarchar) + ', ' + ISNULL(ERROR_MESSAGE(),'')
      SET @Severity = CASE WHEN ERROR_NUMBER() IN(1205,1222) THEN @LockMessageSeverity ELSE 16 END
      RAISERROR(@ErrorMessage,@Severity,1) WITH NOWAIT

      IF NOT (ERROR_NUMBER() IN(1205,1222) AND @LockMessageSeverity = 10)
      BEGIN
        SET @ReturnCode = ERROR_NUMBER()
      END
    END CATCH
  END

  ----------------------------------------------------------------------------------------------------
  --// Log completing information                                                                 //--
  ----------------------------------------------------------------------------------------------------

  SET @EndTime = GETDATE()
  SET @EndTimeSec = CONVERT(datetime,CONVERT(varchar,@EndTime,120),120)

  SET @EndMessage = 'Outcome: ' + CASE WHEN @Execute = 'N' THEN 'Not Executed' WHEN @Error = 0 THEN 'Succeeded' ELSE 'Failed' END
  RAISERROR(@EndMessage,10,1) WITH NOWAIT

  SET @EndMessage = 'Duration: ' + CASE WHEN DATEDIFF(ss,@StartTimeSec, @EndTimeSec)/(24*3600) > 0 THEN CAST(DATEDIFF(ss,@StartTimeSec, @EndTimeSec)/(24*3600) AS nvarchar) + '.' ELSE '' END + CONVERT(nvarchar,@EndTimeSec - @StartTimeSec,108)
  RAISERROR(@EndMessage,10,1) WITH NOWAIT

  SET @EndMessage = 'Date and time: ' + CONVERT(nvarchar,@EndTimeSec,120) + CHAR(13) + CHAR(10) + ' '
  RAISERROR(@EndMessage,10,1) WITH NOWAIT

  IF @LogToTable = 'Y'
  BEGIN
    UPDATE dbo.CommandLog
    SET EndTime = @EndTime,
        ErrorNumber = CASE WHEN @Execute = 'N' THEN NULL ELSE @Error END,
        ErrorMessage = @ErrorMessageOriginal
    WHERE ID = @ID
  END

  ReturnCode:
  IF @ReturnCode <> 0
  BEGIN
    RETURN @ReturnCode
  END

  ----------------------------------------------------------------------------------------------------

END
GO
PRINT 'STEP : Created stored procedure [dbo].[CommandExecute] in master database.'
GO

----------------------------------------------------------------------------------------
-- creating stuff for SQL Server Datbase restore - https://github.com/wetory/SQL-Server-Database-Restore
----------------------------------------------------------------------------------------
USE [master]
GO
IF OBJECT_ID('[dbo].[RestoreDatabase]') IS NOT NULL DROP PROCEDURE [dbo].[RestoreDatabase]
GO
CREATE PROCEDURE [dbo].[RestoreDatabase]

/* 
Purpose: This procedure can be used for regular restores of database that is part of availability group. Taking 
care of all actions needed for proper restore proccess of database in Availability group. It is also writing its actions 
to CommandLog which is able from popular Olla Hallengreen's maintenance.
	
Author:	Tomas Rybnicky
Date of last update: 
	v1.2 - 09.09.2019 - added possiblity to set autogrowth for restored database based on model database settings (RestoreDatabase stored procedure)

List of previous revisions:
	v1.0 - 01.11.2018 - stored procedures cleaned and tested. Solution is usable now.
	v0.1 - 31.10.2018 - Initial solution containing all not necesary scripting from testing and development work
	
Execution example:
	-- restore database and set up autogrowth based on model database
	EXEC [master].[dbo].[RestoreDatabase]
	@BackupFile = N'\\Path\To\BackupFile\Backup.bak',
	@Database	= N'TestDB',
	@LogToTable = 'Y',
	@CheckModel = 'Y'

	-- restore database and add to Availability Group
	EXEC [master].[dbo].[RestoreDatabase]
	@BackupFile = N'\\Path\To\BackupFile\Backup.bak',
	@Database	= N'TestDB',
	@AvailabilityGroup = N'AvailabilityGroupName',
	@SharedFolder = N'\\Path\To\AGShare',
	@LogToTable = 'Y'
*/

@BackupFile			NVARCHAR(1024),			-- Backup file that is to be used for restore
@Database			SYSNAME,				-- Name of restored database
@CheckModel			CHAR(1)			= 'N',	-- Flag if restored database has to attach model database properties (autogrowth for files)
@AvailabilityGroup	SYSNAME			= NULL,	-- Name of Availability Group that is to be used for database. When NULL then normal restore operation happening
@SharedFolder		NVARCHAR(2048)	= NULL,	-- Path to shared network location acessible by all replicas. Required when adding to Availability group
@LogToTable			CHAR(1)			= 'N'	-- Flag if restore commands are to be tracked in CommandLog table

AS

BEGIN
	
	SET NOCOUNT ON
	----------------------------------------------------------------------------------------
	-- declare variables used in script
	----------------------------------------------------------------------------------------
	DECLARE @ErrorMessage			NVARCHAR(MAX)
	DECLARE @InstanceDataPath		VARCHAR(1024)
	DECLARE @InstanceTlogPath		VARCHAR(1024)
	DECLARE @InstanceBackupPath		VARCHAR(1024)
	DECLARE @xp_cmd					VARCHAR(512)
	DECLARE @Version				NUMERIC(18,10)
	DECLARE @Tsql					NVARCHAR(MAX)
	DECLARE @Msg					VARCHAR(MAX)
	DECLARE @PrimaryReplica			SYSNAME
	DECLARE @DatabaseinAG			BIT
	DECLARE @FullBackupPath			NVARCHAR(1024)
	DECLARE @TlogBackupPath			NVARCHAR(1024)

	-- set defaults
	SET @DatabaseinAG = 0

	
	SET @Msg = @@SERVERNAME + ' : Restore database ' + @Database + ' from file ' + @BackupFile
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	SET @Msg =  CHAR(13) + CHAR(10) + 'STEP (' + @@SERVERNAME + '): Checking'
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	----------------------------------------------------------------------------------------
	-- check requirements
	----------------------------------------------------------------------------------------	
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
	-- create tables used in script
	----------------------------------------------------------------------------------------
	IF OBJECT_ID('tempdb..#FileListTable') IS NOT NULL DROP TABLE #FileListTable
	CREATE TABLE #FileListTable (
		[LogicalName]           NVARCHAR(128),
		[PhysicalName]          NVARCHAR(260),
		[Type]                  CHAR(1),
		[FileGroupName]         NVARCHAR(128),
		[Size]                  NUMERIC(20,0),
		[MaxSize]               NUMERIC(20,0),
		[FileID]                BIGINT,
		[CreateLSN]             NUMERIC(25,0),
		[DropLSN]               NUMERIC(25,0),
		[UniqueID]              UNIQUEIDENTIFIER,
		[ReadOnlyLSN]           NUMERIC(25,0),
		[ReadWriteLSN]          NUMERIC(25,0),
		[BackupSizeInBytes]     BIGINT,
		[SourceBlockSize]       INT,
		[FileGroupID]           INT,
		[LogGroupGUID]          UNIQUEIDENTIFIER,
		[DifferentialBaseLSN]   NUMERIC(25,0),
		[DifferentialBaseGUID]  UNIQUEIDENTIFIER,
		[IsReadOnly]            BIT,
		[IsPresent]             BIT,
		[TDEThumbprint]         VARBINARY(32), -- remove this column if using SQL 2005
		[SnapshotUrl]			NVARCHAR(360)
	)

	IF OBJECT_ID('tempdb..#LogicalFilesTable') IS NOT NULL DROP TABLE #LogicalFilesTable
	CREATE TABLE #LogicalFilesTable (
		FileName NVARCHAR(128),
		FileType TINYINT,
		FileId INT,
		FileSize INT
	)

	----------------------------------------------------------------------------------------
	-- check availability group
	----------------------------------------------------------------------------------------
	IF @AvailabilityGroup IS NOT NULL
	BEGIN	
		SET @Msg = ' - availability group'
		RAISERROR(@Msg, 0, 1) WITH NOWAIT;

		-- check if required shared folder given and available 
		IF @SharedFolder IS NULL GOTO SharedFolderNotSpecified

		-- check if HADR enabled
		IF (SELECT SERVERPROPERTY ('IsHadrEnabled')) <> 1 GOTO HadrNotEnabled

		-- check given AG name
		IF NOT EXISTS (SELECT name FROM master.sys.availability_groups WHERE name = @AvailabilityGroup) GOTO UnknownAvailabilityGroup

		-- check primary replica
		SELECT 
			@PrimaryReplica = hags.primary_replica 
		FROM 
			sys.dm_hadr_availability_group_states hags
			INNER JOIN sys.availability_groups ag ON ag.group_id = hags.group_id
		WHERE
			ag.name = @AvailabilityGroup;
		IF @PrimaryReplica <> @@SERVERNAME GOTO NotPrimaryReplica

		-- check if database already part of AG
		SELECT 
			@DatabaseInAG = COUNT(*)			
		FROM 
			master.sys.dm_hadr_database_replica_states drs
			INNER JOIN master.sys.databases db ON drs.database_id = db.database_id
			INNER JOIN master.sys.availability_groups ag ON ag.group_id = drs.group_id
			INNER JOIN master.sys.availability_replicas ar ON ar.replica_id = drs.replica_id
		WHERE replica_server_name = @@SERVERNAME
			AND is_local = 1
			AND is_primary_replica = 1
			AND ag.name = @AvailabilityGroup
			AND db.name = @Database
	END

	SET @Msg = CHAR(13) + CHAR(10) + 'STEP (' + @@SERVERNAME + '): Preparing'
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	----------------------------------------------------------------------------------------
	-- get instance configuration info
	----------------------------------------------------------------------------------------
	SET @Msg = ' - gathering instance info'
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	SET @Version = CAST(LEFT(CAST(SERVERPROPERTY('ProductVersion') AS nvarchar(max)),CHARINDEX('.',CAST(SERVERPROPERTY('ProductVersion') AS nvarchar(max))) - 1) + '.' + REPLACE(RIGHT(CAST(SERVERPROPERTY('ProductVersion') AS nvarchar(max)), LEN(CAST(SERVERPROPERTY('ProductVersion') AS nvarchar(max))) - CHARINDEX('.',CAST(SERVERPROPERTY('ProductVersion') AS nvarchar(max)))),'.','') AS numeric(18,10))
	IF @Version < 10 AND OBJECT_ID('tempdb..#FileListTable') IS NOT NULL ALTER TABLE #FileListTable DROP COLUMN [TDEThumbprint]; 
	IF @Version < 14 AND OBJECT_ID('tempdb..#FileListTable') IS NOT NULL ALTER TABLE #FileListTable DROP COLUMN [SnapshotUrl]; 

	SET @InstanceDataPath = CAST(SERVERPROPERTY('InstanceDefaultDataPath') AS VARCHAR(1024))
	SET @InstanceTlogPath = CAST(SERVERPROPERTY('InstanceDefaultLogPath') AS VARCHAR(1024))
	
	EXEC master.dbo.xp_instance_regread
		N'HKEY_LOCAL_MACHINE',
		N'Software\Microsoft\MSSQLServer\MSSQLServer',
		N'BackupDirectory', 
		@InstanceBackupPath OUTPUT

	----------------------------------------------------------------------------------------
	-- get backup file info
	----------------------------------------------------------------------------------------
	SET @Msg = ' - gathering backup file info'
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	BEGIN TRY
		INSERT INTO #FileListTable EXEC('RESTORE FILELISTONLY FROM DISK = N''' + @BackupFile + '''')
	END TRY
	BEGIN CATCH
		SET @ErrorMessage = ERROR_MESSAGE() + ' Please check if file ' + @BackupFile + ' exists and if not used by another proccess.'
		GOTO QuitWithRollback
	END CATCH	

	----------------------------------------------------------------------------------------
	-- remove database from Availability Group if all requirements are met
	-- requirements:
	--  - need to be called as restore to AG (given by @AvailabilityGroup parameter value)
	--  - instance need to be primary replica
	--  - database need to be already included in AG
	----------------------------------------------------------------------------------------
	IF @AvailabilityGroup IS NOT NULL AND @SharedFolder IS NOT NULL AND @DatabaseinAG = 1
	BEGIN 
		SET @Msg = ' - removing database ' + @Database + ' from Availability Group ' + @AvailabilityGroup
		RAISERROR(@Msg, 0, 1) WITH NOWAIT;

		SET @Tsql = 'ALTER AVAILABILITY GROUP [' + @AvailabilityGroup + '] REMOVE DATABASE [' + @Database + ']'

		EXEC [master].[dbo].[CommandExecute]
		@Command = @Tsql,
		@CommandType = 'AG_REMOVE_DATABASE',
		@DatabaseName = @Database,
		@Mode = 2,
		@LogToTable = @LogToTable,
		@Execute = 'Y'
	END		

	----------------------------------------------------------------------------------------
	-- build restore command
	----------------------------------------------------------------------------------------
	SET @Msg = ' - building restore command'	
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	SET @Tsql = N'RESTORE DATABASE ' + @Database + ' FROM DISK = N''' + @BackupFile + ''' WITH  FILE = 1, NOUNLOAD, REPLACE'

	SELECT @Tsql = @Tsql + 
		CASE 
			WHEN [Type] = 'D' THEN ', MOVE ''' + LogicalName + ''' TO ''' + @InstanceDataPath
			WHEN [Type] = 'L' THEN ', MOVE ''' + LogicalName + ''' TO ''' + @InstanceTlogPath
		END + '\\' + @Database + RIGHT(PhysicalName,4) + ''''
	FROM #FileListTable

	----------------------------------------------------------------------------------------
	-- take database offline and drop it if exist
	----------------------------------------------------------------------------------------
	IF DB_ID(@Database) IS NOT NULL EXECUTE('ALTER DATABASE [' + @Database + '] SET OFFLINE WITH ROLLBACK IMMEDIATE;')
	IF DB_ID(@Database) IS NOT NULL EXECUTE('DROP DATABASE [' + @Database + '];')	
	
	----------------------------------------------------------------------------------------
	-- restore database
	----------------------------------------------------------------------------------------
	SET @Msg = CHAR(13) + CHAR(10) + 'STEP (' + @@SERVERNAME + '): Restoring database'
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	EXEC [master].[dbo].[CommandExecute]
	@Command = @Tsql,
	@CommandType = 'RESTORE_DATABASE',
	@DatabaseName = @Database,
	@Mode = 1,
	@LogToTable = @LogToTable,
	@Execute = 'Y'

	INSERT INTO #LogicalFilesTable EXEC('SELECT [name], [type], [file_id], [size] FROM [' + @Database + '].[sys].[database_files]')

	SET @Msg = 'STEP (' + @@SERVERNAME + '): Post configuration'
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	----------------------------------------------------------------------------------------
	-- set files autogrowth based on model database if given by parameter
	----------------------------------------------------------------------------------------
	IF @CheckModel = 'Y'
	BEGIN 
		SET @Msg = ' - set autogrowth values based on model database'
		RAISERROR(@Msg, 0, 1) WITH NOWAIT;

		DECLARE @DataFileGrowth INT
		DECLARE @LogFileGrowth INT
		DECLARE @DataFileIsPercentGrowth INT
		DECLARE @LogFileIsPercentGrowth INT

		-- gather model database properties
		SELECT 
			@DataFileGrowth = growth,
			@DataFileIsPercentGrowth = is_percent_growth
		FROM master.sys.master_files mf
		INNER JOIN master.sys.databases db ON mf.database_id = db.database_id
		WHERE db.name = 'model' AND mf.type = 0

		SELECT 
			@LogFileGrowth = growth,
			@LogFileIsPercentGrowth = is_percent_growth
		FROM master.sys.master_files mf
		INNER JOIN master.sys.databases db ON mf.database_id = db.database_id
		WHERE db.name = 'model' AND mf.type = 1

		SET @Tsql = ''
		SELECT @Tsql = @Tsql +
			CASE
				WHEN FileType = 0 AND @DataFileIsPercentGrowth = 0 THEN 'ALTER DATABASE [' + @Database + '] MODIFY FILE ( NAME = N''' + FileName + ''', FILEGROWTH = ' + CAST(@DataFileGrowth * 8 AS VARCHAR) + 'KB );'
				WHEN FileType = 0 AND @DataFileIsPercentGrowth = 1 THEN 'ALTER DATABASE [' + @Database + '] MODIFY FILE ( NAME = N''' + FileName + ''', FILEGROWTH = ' + CAST(@DataFileGrowth AS VARCHAR) + '% );'
				WHEN FileType = 1 AND @LogFileIsPercentGrowth  = 0 THEN 'ALTER DATABASE [' + @Database + '] MODIFY FILE ( NAME = N''' + FileName + ''', FILEGROWTH = ' + CAST(@LogFileGrowth * 8 AS VARCHAR) + 'KB );'
				WHEN FileType = 1 AND @LogFileIsPercentGrowth  = 1 THEN 'ALTER DATABASE [' + @Database + '] MODIFY FILE ( NAME = N''' + FileName + ''', FILEGROWTH = ' +CAST( @LogFileGrowth AS VARCHAR) + '% );'
			END
		FROM #LogicalFilesTable
		EXECUTE(@Tsql)
	END

	----------------------------------------------------------------------------------------
	-- shrink log files
	----------------------------------------------------------------------------------------
	SET @Msg = ' - shrink log file'
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	SET @Tsql = 'ALTER DATABASE [' + @Database + '] SET RECOVERY SIMPLE WITH NO_WAIT'
	EXECUTE(@Tsql)

	SET @Tsql = ''
	SELECT @Tsql = @Tsql + 
		CASE
			WHEN (FileSize * 8/1024) > 256 THEN 'USE [' + @Database + ']; DBCC SHRINKFILE (N''' + FileName + ''' , 256);'							-- shrink log file to 256 MB
			WHEN (FileSize * 8/1024) < 256 THEN 'ALTER DATABASE [' + @Database + '] MODIFY FILE ( NAME = N''' + FileName + ''', SIZE = 256MB );'	-- set log file size to 256 MB
		END
	FROM #LogicalFilesTable WHERE FileType = 1
	EXECUTE(@Tsql)

	SET @Tsql = 'ALTER DATABASE [' + @Database + '] SET RECOVERY FULL WITH NO_WAIT'
	EXECUTE(@Tsql)

	----------------------------------------------------------------------------------------
	-- rename logical files
	----------------------------------------------------------------------------------------
	SET @Msg = ' - rename files'	
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	SET @Tsql = 'SET NOCOUNT ON;'

	SELECT @Tsql = @Tsql + 
		'IF NOT EXISTS (SELECT [name] FROM [' + @Database + '].[sys].[database_files] WHERE [name] = ''' +
		@Database  +
		CASE
			WHEN [FileType] = 0 THEN '_Data'
			WHEN [FileType] = 1 THEN '_Log'
		END + 
		CHOOSE(ROW_NUMBER() OVER (PARTITION BY [FileType] ORDER BY [FileId]), '', '_' + CAST(ROW_NUMBER() OVER (PARTITION BY [FileType] ORDER BY [FileId]) AS VARCHAR)) +
		''') ' +
		'ALTER DATABASE ' + @Database + ' MODIFY FILE (NAME=N''' + [FileName] + ''', NEWNAME=N''' + @Database  +
		CASE
			WHEN [FileType] = 0 THEN '_Data'
			WHEN [FileType] = 1 THEN '_Log'
		END + 
		CHOOSE(ROW_NUMBER() OVER (PARTITION BY [FileType] ORDER BY [FileId]), '', '_' + CAST(ROW_NUMBER() OVER (PARTITION BY [FileType] ORDER BY [FileId]) AS VARCHAR)) + 
		''');'
	FROM #LogicalFilesTable
	ORDER BY [FileType]

	EXECUTE(@Tsql)

	----------------------------------------------------------------------------------------
	-- set database to multi user mode
	----------------------------------------------------------------------------------------
	SET @Msg = ' - set multi user'
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	IF DB_ID(@Database) IS NOT NULL EXEC('ALTER DATABASE [' + @Database + '] SET MULTI_USER')

	----------------------------------------------------------------------------------------
	-- set database to online mode
	----------------------------------------------------------------------------------------
	SET @Msg = ' - set online'
	RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	IF DB_ID(@Database) IS NOT NULL EXEC('ALTER DATABASE [' + @Database + '] SET ONLINE')

	----------------------------------------------------------------------------------------
	-- take full backup and backup of transaction log if database to be included in AG
	----------------------------------------------------------------------------------------
	IF @AvailabilityGroup IS NOT NULL AND @SharedFolder IS NOT NULL 
	BEGIN	
		SET @Msg = CHAR(13) + CHAR(10) + 'STEP (' + @@SERVERNAME + '): Add database ' + @Database + ' to Availability Group ' + @AvailabilityGroup
		RAISERROR(@Msg, 0, 1) WITH NOWAIT;
		
		-- full backup
		SET @Msg = ' - take full backup'
		RAISERROR(@Msg, 0, 1) WITH NOWAIT;

		SET @FullBackupPath = @SharedFolder + '\' + @Database + '_AG_init.bak'
		SET @Tsql = 'BACKUP DATABASE [' + @Database +'] TO  DISK = N''' + @FullBackupPath + ''' WITH  FORMAT, INIT, SKIP, REWIND, NOUNLOAD'

		EXEC [master].[dbo].[CommandExecute]
		@Command = @Tsql,
		@CommandType = 'BACKUP_DATABASE',
		@DatabaseName = @Database,
		@Mode = 2,
		@LogToTable = @LogToTable,
		@Execute = 'Y'

		-- backup of transaction log
		SET @Msg = ' - take backup of transaction log'
		RAISERROR(@Msg, 0, 1) WITH NOWAIT;

		SET @TlogBackupPath = @SharedFolder + '\' + @Database + '_' + FORMAT( GETDATE(), 'yyyyMMddHHmmss') + '.trn'
		SET @Tsql = 'BACKUP LOG [' + @Database +'] TO  DISK = N''' + @TlogBackupPath + ''' WITH NOFORMAT, NOINIT, NOSKIP, REWIND, NOUNLOAD'

		EXEC [master].[dbo].[CommandExecute]
		@Command = @Tsql,
		@CommandType = 'BACKUP_LOG',
		@DatabaseName = @Database,
		@Mode = 2,
		@LogToTable = @LogToTable,
		@Execute = 'Y'
		
	END

	----------------------------------------------------------------------------------------
	-- add database to availability group on primary replica
	----------------------------------------------------------------------------------------
	IF @AvailabilityGroup IS NOT NULL AND @SharedFolder IS NOT NULL 
	BEGIN
		SET @Msg = ' - add on primary replica ' + @@SERVERNAME
		RAISERROR(@Msg, 0, 1) WITH NOWAIT;

		SET @Tsql = 'ALTER AVAILABILITY GROUP [' + @AvailabilityGroup + '] ADD DATABASE [' + @Database + '];'

		EXEC [master].[dbo].[CommandExecute]
		@Command = @Tsql,
		@CommandType = 'AG_JOIN_PRIMARY',
		@DatabaseName = @Database,
		@Mode = 2,
		@LogToTable = @LogToTable,
		@Execute = 'Y'
	END

	----------------------------------------------------------------------------------------
	-- add database to availability group on every secondary replica
	----------------------------------------------------------------------------------------
	IF @AvailabilityGroup IS NOT NULL AND @SharedFolder IS NOT NULL 
	BEGIN
		-- gather all secondary replicas
		IF OBJECT_ID('tempdb..#SecondaryReplicas') IS NOT NULL DROP TABLE #SecondaryReplicas
		CREATE TABLE #SecondaryReplicas (
			ReplicaId INT IDENTITY(1,1) PRIMARY KEY,
			ReplicaName NVARCHAR(256),
			Processed BIT DEFAULT 0
		)

		INSERT INTO #SecondaryReplicas(ReplicaName)
		SELECT ar.replica_server_name
		FROM master.sys.dm_hadr_availability_group_states hags
			INNER JOIN master.sys.availability_replicas ar ON ar.group_id = hags.group_id
			INNER JOIN master.sys.availability_groups ag ON ag.group_id = hags.group_id
		WHERE
			ag.name = @AvailabilityGroup
			AND ar.replica_server_name NOT LIKE hags.primary_replica

		-- iterate through secodary replicas
		DECLARE @CurrentReplicaId INT
		DECLARE @CurrentReplicaName NVARCHAR(256)

		WHILE EXISTS(SELECT * FROM #SecondaryReplicas WHERE Processed = 0)
		BEGIN
			SELECT TOP 1 
				@CurrentReplicaId = ReplicaId, 
				@CurrentReplicaName = ReplicaName
			FROM #SecondaryReplicas
			WHERE Processed = 0
			ORDER BY ReplicaId ASC

			--check if linked server to the secondary replica exists and add it if not
			IF NOT EXISTS ( SELECT TOP (1) * FROM master.sys.sysservers WHERE srvname = @CurrentReplicaName AND srvid <> 0 ) 
			BEGIN
				SET @Msg = ' - creating linked server for ' + @CurrentReplicaName + ' replica'
				RAISERROR(@Msg, 0, 1) WITH NOWAIT;

				EXEC master.dbo.sp_addlinkedserver @server = @CurrentReplicaName, @srvproduct=N'SQL Server'

				SET @Msg = ' - enabling RPC for linked server ' + @CurrentReplicaName
					RAISERROR(@Msg, 0, 1) WITH NOWAIT;

				EXEC master.dbo.sp_serveroption @server = @CurrentReplicaName, @optname=N'rpc out', @optvalue=N'true'
			END
			ELSE
			BEGIN
				-- ensure that RPC is enabled for linked server							
				IF NOT EXISTS ( SELECT TOP (1) * FROM master.sys.sysservers WHERE srvname = @CurrentReplicaName AND srvid <> 0 and rpcout = 1) 
				BEGIN
					SET @Msg = ' - enabling RPC Out for linked server ' + @CurrentReplicaName
					RAISERROR(@Msg, 0, 1) WITH NOWAIT;

					EXEC master.dbo.sp_serveroption @server = @CurrentReplicaName, @optname=N'rpc out', @optvalue=N'true'
				END
			END

			SET @Msg = ' - add on secondary replica ' + @CurrentReplicaName
			RAISERROR(@Msg, 0, 1) WITH NOWAIT;

			-- check if add database secondary procedure exists on secondary replica
			DECLARE @i BIT

			SET @Tsql = N'SELECT @Exists = COUNT(*) FROM [' + @CurrentReplicaName + '].[master].[sys].[objects] WHERE type = ''P'' AND name  = ''AddDatabaseOnSecondary'''

			EXEC sp_executesql
				@Tsql,
				N'@Exists INT OUTPUT',
				@i OUTPUT
			
			IF  @i = 0
			BEGIN
				SET @ErrorMessage = 'Stored procedure [master].[dbo].[AddDatabaseOnSecondary] not found on server ' + @CurrentReplicaName + ' or execution account does not have sufficient permissions. Please check procedure and account permissions and rerun or add database to secondary manually. Exitting...'
				GOTO QuitWithRollback
			END
			ELSE
			BEGIN
				-- lets execute procedure then
				SET @Tsql = 'EXEC [' + @CurrentReplicaName + '].[master].[dbo].[AddDatabaseOnSecondary]
				@FullBackupFile = N''' + @FullBackupPath + ''',
				@TlogBackupFile = N''' + @TlogBackupPath + ''',
				@Database = N''' + @Database + ''',
				@AvailabilityGroup = N''' + @AvailabilityGroup + ''',
				@LogToTable = ''Y'''

				EXEC(@Tsql)
			END

			UPDATE #SecondaryReplicas
			SET Processed = 1
			WHERE ReplicaId = @CurrentReplicaId

			SET @CurrentReplicaId = NULL
			SET @CurrentReplicaName = NULL
		END

		SET @Msg = CHAR(13) + CHAR(10) +  'STEP (' + @@SERVERNAME + '): Joining database ' + @Database + ' to all secondary replicas finished '
		RAISERROR(@Msg, 0, 1) WITH NOWAIT;
	END
	
	----------------------------------------------------------------------------------------
	-- finish
	----------------------------------------------------------------------------------------
	GOTO Finish

	----------------------------------------------------------------------------------------
	-- skip restore because HADR is not enabled on instance
	----------------------------------------------------------------------------------------
	HadrNotEnabled:
		SET @ErrorMessage = 'HADR not enabled on instance ' + @@SERVERNAME + ', use normal restore instead of restore to AG. Exitting...'
		GOTO QuitWithRollback

	----------------------------------------------------------------------------------------
	-- skip restore because wrong Availabilit Group name given
	----------------------------------------------------------------------------------------
	UnknownAvailabilityGroup:
		SET @ErrorMessage = 'Availability group ' + @AvailabilityGroup + ' not found! Check input parameters and try again. Exitting...'
		GOTO QuitWithRollback

	----------------------------------------------------------------------------------------
	-- skip restore because this server is not primary replica
	----------------------------------------------------------------------------------------
	NotPrimaryReplica:
		SET @Msg = 'Server ' + @@SERVERNAME + ' is not primary replica of Availability Group ' + @AvailabilityGroup + '! Exitting...'
		RAISERROR(@Msg, 0, 1) WITH NOWAIT;
		GOTO EndOfFile

	----------------------------------------------------------------------------------------
	-- handle error when @SharedFolder parameter not given when adding to Availability Group
	----------------------------------------------------------------------------------------
	SharedFolderNotSpecified:
		SET @ErrorMessage = 'Availability Group ' + @AvailabilityGroup + ' name specified, but parameter @SharedFolder is missing! Shared folder location needed to add database to Availability Group. Exitting...'
		GOTO QuitWithRollback

	----------------------------------------------------------------------------------------
	-- handle error message
	----------------------------------------------------------------------------------------
	QuitWithRollback:
		IF (@@TRANCOUNT > 0) ROLLBACK TRANSACTION
		RAISERROR(@ErrorMessage, 16, 1) WITH NOWAIT
		GOTO EndOfFile

	----------------------------------------------------------------------------------------
	-- just finishing script
	----------------------------------------------------------------------------------------
	Finish:
		IF @AvailabilityGroup IS NOT NULL
		BEGIN
			SET @Msg = CHAR(13) + CHAR(10) + 'Database ' + @Database + ' sucessfully restored on server ' + @@SERVERNAME + ', and joined Availability Group ' + @AvailabilityGroup	
		END
		ELSE
		BEGIN
			SET @Msg = CHAR(13) + CHAR(10) + 'Database ' + @Database + ' sucessfully restored on server ' + @@SERVERNAME	
		END		
		RAISERROR(@Msg, 0, 1) WITH NOWAIT;

	----------------------------------------------------------------------------------------
	-- put any cleanup stuff here as script will always hit this part
	----------------------------------------------------------------------------------------
	EndOfFile:
		IF OBJECT_ID('tempdb..#FileListTable')		IS NOT NULL DROP TABLE #FileListTable
		IF OBJECT_ID('tempdb..#LogicalFilesTable')	IS NOT NULL DROP TABLE #LogicalFilesTable
		IF OBJECT_ID('tempdb..#SecondaryReplicas') IS NOT NULL DROP TABLE #SecondaryReplicas	
END
GO
PRINT 'STEP : Created stored procedure [dbo].[RestoreDatabase] in master database.'
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
GO
PRINT 'STEP : Created stored procedure [dbo].[AddDatabaseOnSecondary] in master database.'
GO
PRINT '-------------------------------------------------------------------------'
PRINT 'SQL Server Database Restore - deployed to ' + @@SERVERNAME 