  OMIT('_EndOfRifDateInclude_',_RifDateIncluded_)
_RifDateIncluded_ EQUATE(1)


RifDateClass  CLASS,TYPE,MODULE('clsRifDate.clw'),LINK('clsRifDate.clw',1), DLL(0)
initialized                 BOOL,PROTECTED
    
RC            LONG
OutputString  CSTRING(50000)
InputString   CSTRING(50000) 
Month         LONG
MonthText3    STRING(3)
MonthText     STRING(20)

intToday      LONG
intThisYear   LONG
intNextYear   LONG
intLastYear   LONG
int2YearsAgo  LONG 

! for DetermineTimeZone and ConvertUTCToLocalTime
strTZone             STRING(3)
intBias              LONG
strTZType            STRING(2)
strTZInitial         STRING(1) 
intDaylightBias      LONG                                  ! 
intDaylightDisabled  LONG  

!--- protected methods
Construct                   PROCEDURE(),PROTECTED
Destruct                    PROCEDURE(),PROTECTED
Debug                       PROCEDURE(STRING pDebugMessage)


!--- Public Methods
IsInitialized               PROCEDURE(),BOOL
MonthsBetween               PROCEDURE(LONG pFromDate, LONG pToDate),LONG,PROC
MonthYearToDate             PROCEDURE(LONG pMonth, LONG pYear),LONG,PROC
SetMonth                    PROCEDURE(LONG pMonth),bool,proc
SetDate                     PROCEDURE(LONG pDate),BOOL,proc

ReturnClarionDate           PROCEDURE (*CSTRING pFromDate),LONG
ReturnClarionDate           PROCEDURE (STRING pFromDate),LONG 
ReturnSQLTime               PROCEDURE(LONG pClarionTime),STRING 
ReturnClarionTime           PROCEDURE (*CSTRING pFromTime),LONG 
ReturnClarionTime           PROCEDURE (STRING pFromTime),LONG 

UnixFormattedDateTimeToClarion PROCEDURE (STRING pFromDate, *LONG pToDate, <*LONG pToTime>, <*STRING pTimeZone>)

SetTodayCommand             PROCEDURE()
SetToday                    PROCEDURE(*LONG pintToday)
SetToday                    PROCEDURE()

GetLastXXXDayOfTheMonth     PROCEDURE(LONG pDate, LONG pWhichWeekDay),LONG
DaysInMonth                 PROCEDURE(LONG pDate),LONG 

DetermineTimeZone           PROCEDURE()
ConvertUTCToLocalTime       PROCEDURE(LONG pDate, LONG pTime, *LONG pLocalDate, *LONG pLocalTime)
ConvertUTCToLocalTime       PROCEDURE(DATE pDate, TIME pTime, *DATE pLocalDate, *TIME pLocalTime)

             End

!_EndOfRifDateInclude_