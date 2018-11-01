/*  
	Title: SQL Server Database Restore - deployment script
	Description: Script is creating standardized restore procedures that can be used for regular restored of database, for example from PROD to DEV environment. Highly tied with
	from Olla Hallengreen's maintenance solution https://ola.hallengren.com/ as it is using its CommandLog table and CommandExecute stored procedure. So can and should co-exist 
	together with this maintenance on same instance :)

	It is creating following stuff in SQL Server instance:
		- stored procedures in master database

	Author: Tomas Rybnicky 
	Date of last update: 
	 v1.0 - 01.11.2018 - stored procedures cleaned and tested. Solution is usable now.

	List of previous revisions:
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
PRINT 'STEP : Created stored procedure [dbo].[AddDatabaseOnSecondary] in master database.'
GO
PRINT 'STEP : Created stored procedure [dbo].[RestoreDatabase] in master database.'
GO
PRINT '-------------------------------------------------------------------------'
PRINT 'SQL Server Database Restore - deployed to ' + @@SERVERNAME 