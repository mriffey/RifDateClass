
  MEMBER
  
  INCLUDE('clsRifDate.inc'),ONCE

OMIT('-++--',Time:Once)
Time:Once     EQUATE(1)
Time:Seconds  EQUATE(100)
Time:Minutes  EQUATE(60*Time:Seconds)
Time:Hours    EQUATE(60*Time:Minutes)  
Time:EOD      EQUATE(23*Time:Hours + 59 * Time:Minutes + 59 * Time:Seconds + 100)  
-++--

  MAP
    MODULE('Win32')
      OutputDebugString(*CString),raw,pascal,name('OutputDebugStringA')
    END
  END
  
strToday                STRING(20),THREAD 
intLoop                 LONG,THREAD

!-------------------------------------------------------
RifDateClass.DetermineTimeZone             PROCEDURE()

 CODE 
 
 SELF.strTZInitial        = GetReg(REG_LOCAL_MACHINE,'SYSTEM\CurrentControlSet\Control\TimeZoneInformation','TimeZoneKeyName')
 SELF.intBias             = GetReg(REG_LOCAL_MACHINE,'SYSTEM\CurrentControlSet\Control\TimeZoneInformation','Bias')
 SELF.intDaylightDisabled = GetReg(REG_LOCAL_MACHINE,'SYSTEM\CurrentControlSet\Control\TimeZoneInformation','DynamicDaylightTimeDisabled')
 SELF.intDaylightBIas     = GetReg(REG_LOCAL_MACHINE,'SYSTEM\CurrentControlSet\Control\TimeZoneInformation','ActiveTimeBias')
 
 IF SELF.intDaylightDisabled = 1
    SELF.intBias = SELF.intDaylightBias 
    !ud.debug('TZ Using Standard time') 
    SELF.strTZType = 'ST'
 ELSE
    !ud.debug('TZ Using Daylight time')
    SELF.strTZType = 'DT'
 END 
 
 SELF.intBias  = SELF.intBias * time:minutes                       
 SELF.strTZone = SELF.strTZInitial & SELF.strTZType
 
 RETURN 

!-------------------------------------------------------
RifDateClass.ConvertUTCToLocalTime         PROCEDURE(DATE pDate, TIME pTime, *DATE pLocalDate, *TIME pLocalTime)

intTempRDCDateIn  LONG 
intTempRDCTimeIn  LONG 
intTempRDCDateOut LONG 
intTempRDCTimeOut LONG 

 CODE 

 intTempRDCDateIn = pDate
 intTempRDCTimeIn = pTime 
 SELF.ConvertUTCToLocalTime(intTempRDCDateIn, intTempRDCTimeIn, intTempRDCDateOut, intTempRDCTimeOut)
 pLocalDate = intTempRDCDateOut
 pLocalTime = intTempRDCTimeOut
 RETURN 
 

!-------------------------------------------------------
RifDateClass.ConvertUTCToLocalTime         PROCEDURE(LONG pDate, LONG pTime, *LONG pLocalDate, *LONG pLocalTime)

 CODE 
 
 IF pTime > 0
    pLocalTime = pTime - SELF.intBias 
 ELSE
    pLocalTime = 0
 END 

 IF pLocalTime < 0 
    pLocalTime = Time:EOD - ABS(pTime)
    pLocalDate = pLocalDate - 1
 ELSE
    pLocalDate = pDate 
 END        
 
 RETURN 
 
!-------------------------------------------------------
RifDateClass.DaysInMonth                   PROCEDURE(LONG pDate) !,LONG 

intWorkDate    LONG 
intDaysInMonth LONG 

 CODE 
 
 intDaysInMonth = -1 
 
 IF pDate > 0
    intWorkDate    = pDate 
    intDaysInMonth = -1
 ELSE
    RETURN intDaysInMonth 
 END 
 
 IF intWorkDate > 12
    intWorkDate = MONTH(intWorkDate)
 END 
 
 CASE intWorkDate
    OF 1
    OROF 3
    OROF 5
    OROF 7
    OROF 8
    OROF 10
    OROF 12
         intDaysInMonth = 31
       
    OF 2
       IF YEAR(pDate) % 4 = 0 
       ! It's a leap year if the year is divisible by 4, except for year's divisible by 100, which are not a leap year unless divisible by 400. 
       ! IE: 2000 was a leap year. 2100, 2200, and 2300 are not leap years. 2400 is a leap year. 
          intDaysInMonth =  29          

          IF YEAR(pDate) % 100 = 0          
             IF YEAR(pDate) % 400 = 0          
             ELSE 
                intDaysInMonth = 28
             END 
          END 
       ELSE
          intDaysInMonth =  28
       END 
       
    OF 4
    OROF 6
    OROF 9
    OROF 11
         intDaysInMonth =  30
 END 
          
 RETURN intDaysInMonth
 
!-------------------------------------------------------------------
! Gets the date of the last <Sunday or Monday or ...> of the month in pDate
! pWhichWeekDay = 0 = Sunday, 1 = Monday, 2 = Tuesday, 3 = Wednesday, 4 = Thursday, 5 = Friday, 6 = Saturday
!-------------------------------------------------------------------

RifDateClass.GetLastXXXDayOfTheMonth       PROCEDURE(LONG pDate, LONG pWhichWeekDay)! ,LONG 
intMonth              LONG 
intDayOfWeek          LONG 
intLastDayOfTheMonth  LONG 
intReturnDate         LONG 

 CODE 
 
 intReturnDate = -1 
 
 IF pDate > 0
 ELSE
    RETURN -1 
 END 
 
 IF pWhichWeekDay > -1 AND pWhichWeekDay < 7
 ELSE 
    RETURN -1
 END 
 
 intMonth = MONTH(pDate) 
 intLastDayOfTheMonth = DATE(intMonth,SELF.DaysInMonth(pDate), YEAR(pDate))
 
 LOOP intLoop = intLastDayOfTheMonth TO 1 BY -1
    intDayOfWeek = intLoop % 7
    IF intDayOfWeek = pWhichWeekDay
       intReturnDate = DATE(MONTH(pDate),intLoop,YEAR(pDate))       
       BREAK
    END 
 END 
 
 RETURN intReturnDate
 
!----------------------------------------------------------------
!
!----------------------------------------------------------------
RifDateClass.debug            PROCEDURE  (String pODStext)             
sZtext &Cstring
  CODE
  szText &= New CSTRING(Len(pODSText)+1)

  If Not szText &= NULL
    szText = pODStext
    OutputDebugString(szText)
    Dispose(szText)
  End
  Return

!----------------------------------------------------------------
RifDateClass.SetMonth             PROCEDURE(LONG pMonth)!,bool,proc
   CODE

   IF pMonth > 0 AND pMonth < 12
      SELF.Month = pMonth
      SELF.MonthText3 = Choose(self.Month,'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec') 
      SELF.MonthText  = Choose(self.Month,'January','February','March','April','May','June','July','August','September','October','November','December')
   END
  
   RETURN(TRUE)

!----------------------------------------------------------------
RifDateClass.SetDate              PROCEDURE(LONG pDate)!,BOOL,proc
   CODE

   SELF.SetMonth(MONTH(pDate))
   !SELF.SetDay(DAY(pDate))
   RETURN(TRUE)

!---------------------------------------------------------------- 
RifDateClass.MonthYearToDate      PROCEDURE(LONG pMonth, LONG pYear)!,LONG,PROC

ReturnedDate LONG

  CODE
  ReturnedDate = DATE(pMonth,1,pYear)
  Return(ReturnedDate)


!----------------------------------------------------------------   
! wrap the passed text in dblquotes. return no string if blank
RifDateClass.MonthsBetween        PROCEDURE(LONG pFromDate,LONG pToDate)!,LONG,proc

MonthCount LONG(0)
YearDiff   LONG(0)
DaysDiff   LONG(0)
StartDate  LONG(0)
EndDate    LONG(0)
WorkDate   LONG

  CODE
 
  IF pToDate > pFromDate
     StartDate = pFromDate
     EndDate   = PToDate
  else     
     StartDate = pToDate
     EndDate   = pFromDate
  END

  WorkDate = StartDate
  LOOP 
     WorkDate = DATE(MONTH(WorkDate) + 1,1,YEAR(WorkDate))
     IF WorkDate > EndDate 
        BREAK
     ELSE 
        MonthCount += 1 
     END 
  END

  MonthCount -=1 !dont count the current month
  !MESSAGE(monthcount) 
  RETURN(MonthCount)

!----------------------------------------------------------------  
RifDateClass.IsInitialized       Procedure()!,BOOL

  CODE
  
  RETURN SELF.initialized

!----------------------------------------------------------------------------------------------------------------
RifDateClass.SetTodayCommand  PROCEDURE()
!----------------------------------------------------------------------------------------------------------------
 CODE 
  
 IF INSTRING('today',LOWER(CLIP(COMMAND(''))),1,1) > 0
    strToday = COMMAND('today') !YYYYMMDD
    SELF.intToday = DATE(strToday[5 : 6],strToday[7 : 8],strToday[1 : 4])       
    !SELF.Debug('Today set to ' & CLIP(COMMAND('today')) & ' ' & FORMAT(SELF.intToday,@D10-))
 ELSE
    SELF.SetToday()
    !SELF.Debug('Today set to ' & FORMAT(SELF.intToday,@D10-))
 END 
 
 RETURN 
 
!----------------------------------------------------------------------------------------------------------------
RifDateClass.SetToday  PROCEDURE(*LONG pintToday)
!----------------------------------------------------------------------------------------------------------------
 CODE 
 
 IF pintToday > 0
    SELF.intToday = pintToday 
 END 
 
 RETURN 
 
!----------------------------------------------------------------------------------------------------------------
RifDateClass.SetToday  PROCEDURE()
!----------------------------------------------------------------------------------------------------------------
 CODE 
 
 SELF.intToday = TODAY() 
 
 RETURN 
 
!----------------------------------------------------------------------------------------------------------------
RifDateClass.Construct  PROCEDURE()
!----------------------------------------------------------------------------------------------------------------

 CODE

 SELF.SetTodayCommand() 
 
 SELF.intThisYear  = YEAR(SELF.intToday)
 SELF.intNextYear  = YEAR(SELF.intToday) + 1
 SELF.intLastYear  = YEAR(SELF.intToday) - 1
 SELF.int2YearsAgo = YEAR(SELF.intToday) - 2
 
 SELF.DetermineTimeZone() 

 Self.initialized = TRUE ! Choose(Self.iCS &=Null, FALSE, TRUE) 
 RETURN



!-------------------------------------------------------------------
RifDateClass.Destruct            PROCEDURE()

  CODE
 
  RETURN

!-----------------------------------------------------------
! UnixFormattedToClarionDate returns a LONG Clarion date given a standard Unix Date or DateTime string 
!----------------------------------------------------------------------------------------------------------------
RifDateClass.UnixFormattedDateTimeToClarion PROCEDURE (STRING pFromDate, *LONG pToDate, <*LONG pToTime>, <*STRING pTimeZone>)
!----------------------------------------------------------------------------------------------------------------
strDate         STRING(25) 
strTime         STRING(20)
dDate           DATE 
intDate         LONG 
intTZ           LONG 

 CODE
 
 ! 2014-10-21T23:35:38-07:00
 ! 0000000001111111111222222
 ! 1234567890123456789012345
  
 strDate = pFromDate
 pToDate = SELF.ReturnClarionDate(strDate)  ! this is a D10 date
 
 IF OMITTED(pToTime)
 ELSE
    IF LEN(CLIP(pFromDate)) > 18  
       strTime = pFromDate[12:19]
       pToTime = SELF.ReturnClarionTime(strTime)
       IF LEN(CLIP(pFromDate)) = 25
          CASE pFromDate[20:25]
             OF '-07:00'
                pTimeZone = 'PT'
             OF '-06:00'
                pTimeZone = 'MT'
             OF '-05:00'
                pTimeZone = 'CT'
             OF '-04:00'
                pTimeZone = 'ET'
             OF '-00:00'
                pTimeZone = 'UTC'
             ELSE
                pTimeZone = ''
          END
       END 
    ELSE
       pToTime = 0
    END  
 END
 
 RETURN 
 
!-----------------------------------------------------------
! ReturnClarionDate returns a LONG Clarion date given a standard SQL date (YYYY-MM-DD)
!----------------------------------------------------------------------------------------------------------------
RifDateClass.ReturnClarionDate PROCEDURE (*CSTRING pFromDate)!,LONG
!----------------------------------------------------------------------------------------------------------------
strDate         STRING(10) 
dDate           DATE 
intDate         LONG 

 CODE
 
 strDate = pFromDate
 intDate   = SELF.ReturnClarionDate(strDate)
 RETURN intDate 
 
!----------------------------------------------------------------------------------------------------------------
RifDateClass.ReturnClarionDate PROCEDURE (STRING pFromDate)!,LONG 
!----------------------------------------------------------------------------------------------------------------
dDate           DATE 
intDate         LONG
FirstDash       LONG
myMonth         LONG
myDay           LONG
myYear          LONG

 CODE
 
 intDate = 0
 FirstDash = INSTRING('-',pFromDate,1,1)
 IF CLIP(pFromDate) > ' ' AND FirstDash > 0 
 ELSE
    RETURN intDate 
 END
 CASE FirstDash
   OF 3  !- mm-dd-yyyy
      myYear      = pFromDate[7 : 10]
      myMonth     = pFromDate[1 : 2]
      myDay       = pFromDate[4 : 5]
   OF 5  !- yyyy-mm-dd
      myYear      = pFromDate[1 : 4]
      myMonth     = pFromDate[6 : 7]
      myDay       = pFromDate[9 : 10]
 END
 intDate    = DATE(myMonth, myDay, myYear)
 RETURN intDate 
 
!----------------------------------------------------------------------------------------------------------------  
RifDateClass.ReturnSQLTime                     PROCEDURE(LONG pClarionTime)!,STRING 
!---------------------------------------------------------------------------------------------------------------- 
strSQLTime STRING(10)

 CODE
 
 IF pClarionTime > 0
    strSQLTime = FORMAT(pClarionTime,@T05)
 ELSE
    CLEAR(strSQLTime)
 END 
 RETURN strSQLTime 
 
!---------------------------------------------------------------------------------------------------------------- 
RifDateClass.ReturnClarionTime PROCEDURE (*CSTRING pFromTime)!,LONG 
!----------------------------------------------------------------------------------------------------------------
strTime         STRING(10) 
intTime         LONG 

 CODE
 
 strTime = pFromTime 
 intTime = SELF.ReturnClarionTime(strTime) 
 RETURN intTime 
 
!----------------------------------------------------------------------------------------------------------------
RifDateClass.ReturnClarionTime PROCEDURE (STRING pFromTime)!,LONG 
!----------------------------------------------------------------------------------------------------------------
intTime         LONG 
cwHour          EQUATE(360000)  !- cwMinute * 60
cwMinute        EQUATE(6000)    !- cwSecond * 60
cwSecond        EQUATE(100)     !- 100
myHours         LONG
myMinutes       LONG
mySeconds       LONG

 CODE
 
 CLEAR(intTime)  
 IF LEN(pFromTime) > 0 AND CLIP(pFromTime) > ' ' AND INSTRING(':',CLIP(pFromTime),1,1) > 0
    myHours     = pFromTime[1:2]
    myMinutes   = pFromTime[4:5]
    mySeconds   = pFromTime[7:8]
    intTime       = (myHours * cwHour) + (myMinutes * cwMinute) + (mySeconds * cwSecond) + 1 
 END
 RETURN intTime  