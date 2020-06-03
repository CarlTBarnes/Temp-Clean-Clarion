!region Notices
! ================================================================================
! MIT License
!
! Copyright (c) 2020 Carl T. Barnes
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
! 
! WARNING: This program deletes files and they cannot be recovered.
! ================================================================================
!endregion Notices

  PROGRAM


YES_Delete_Files   EQUATE(0) !<--- When (1) DeleteFile() actually happens, else outputs debug only
                   !^^^^^^^^
                   
! [ ] Test it out before you set YES_Delete as (1)
! [ ] Suggest you put your Company Name in the Window Caption.
                  
                  
! Run with /VIEW commandline to see the files before Cleaning and have to press GO button. CW Project is set this way.
!
! CAUTION: This does DELETE files from the TEMP folder. You MUST verify it works as desired or you RISK losing files

! What does this Delete?
!   1. *.Wmf        files zero bytes in size
!   2. [0-9]*.Wmf   files named 12345678*.WMF   CPCS creates WMF with all numeric names that are not always purged.
!   3. CLA*.Tmp     Report preview WMF made by RTF
!   3. PDF*.Tmp     Tracker makes these files
!
! Files must be over 4 days old to be purged, in case use has left report open.

!region How to use:
!   When the Frame Window close every 14 days run this to do house keeping. The first time it may find MANY files
!   
!    RunTempCleanRtn ROUTINE
!        DATA
!    DateLast    LONG
!        CODE
!        !INI file MUST be Per User. Better to use GETREG(REG_CURRENT_USER
!        DateLast=GETINI('TempClean','DateLast',0,WinPosIni)   
!        IF ~DateLast OR TODAY()-DateLast > 14 THEN
!            RUN('TempClean.EXE')  
!            ! add /view to see files before and have GO button.  add /wait to pause at end
!            PUTINI('TempClean','DateLast',TODAY(),WinPosIni)
!        END
!endregion     

!=======================================================================
  INCLUDE 'KEYCODES.CLW'
  MAP
TempPurgeTmp    PROCEDURE()  
DB              PROCEDURE(STRING xMessage)    
    MODULE('Win32')
        GetTempPath(LONG nBufferLength,*CSTRING lpTempPath),LONG,PASCAL,DLL(1),RAW,NAME('GetTempPathA'),PROC
        DeleteFile(*CSTRING lpFileName ),BOOL,RAW,PASCAL,DLL(1),PROC,name('DeleteFileA')
        GetLastError(),Long,PASCAL,DLL(1)
        OutputDebugString(*cstring Msg),PASCAL,RAW,NAME('OutputDebugStringA'),DLL(1)        
    END          
  END
IsVIEW  BYTE !/View = Pause to view files with GO button, also waits at end
IsWAIT  BYTE !/wait = Purge but do NOT close at end 
  CODE
  IF INSTRING('view',lower(command('')),1) THEN IsVIEW=1. !Run /VIEW to not Purge
  IF INSTRING('wait',lower(command('')),1) THEN IsWAIT=1. !Run /WAIT to not Purge
  IF ~IsVIEW AND ~IsWAIT AND ~INSTRING('wait',lower(command('')),1) THEN
  END   
  TempPurgeTmp()
!-----------
TempPurgeTmp    PROCEDURE()
WinTempBS  CSTRING(256)
cFileName  CSTRING(261)

QNdx    LONG,AUTO
DirQ    QUEUE(FILE:Queue),PRE(DirQ) . ! DirQ:Name  DirQ:ShortName(8.3?)  DirQ:Date  DirQ:Time  DirQ:Size  DirQ:Attrib

SortNow     SHORT(1)
SortLast    SHORT
SortNowWho  STRING(128)
SortLastWho STRING(128)
ProgNdx     LONG
ProgPct     LONG
ProgPctNew  LONG
ProgRecords LONG
ProgRatio   REAL

FilesWindow WINDOW('Temp Folder Cleanup'),AT(,,320,130),CENTER,GRAY,SYSTEM, |   !,ICON('Mark2.ICO')
            FONT('Segoe UI',9,,FONT:regular),RESIZE
        BUTTON('Go'),AT(0,0,14,12),USE(?GoBtn),TIP('IsVIEW=1'),HIDE
        PROGRESS,AT(17,2,286,9),USE(ProgPct),RANGE(0,100)
        LIST,AT(1,15),FULL,USE(?ListFiles),VSCROLL,TIP('Click heads to sort, Right Click to reverse,' & |
                ' Ctrl+C to Copy'),FROM(DirQ),FORMAT('138L(1)|M~Name~@s255@52L(1)|M~EXT~@s13@40R(1)|' & |
                'M~Date~C(0)@d1@40R(1)|M~Time~C(0)@T4b@46R(1)|M~Size~C(0)@n13@'),ALRT(CtrlC)
    END
Dir2Clp     ANY
DateCutoff  LONG
LenFN       USHORT
Exten       STRING(4) 
    CODE
    IF ~GetTempPath(SIZE(WinTempBS),WinTempBS) THEN RETURN.
    IF ~WinTempBS OR ~EXISTS(WinTempBS) THEN RETURN.
    DateCutoff=TODAY()-4
    DB('TempFolder: ' & WinTempBS & '  YES_Delete_Files=' & YES_Delete_Files )
    DB('DateCutoff: ' & FORMAT(DateCutoff,@d2))
    FREE(DirQ)
    DIRECTORY(DirQ,WinTempBS&'*.tmp',ff_:NORMAL)
    DIRECTORY(DirQ,WinTempBS&'*.wmf',ff_:NORMAL)

    LOOP QNdx = RECORDS(DirQ) TO 1 BY -1
         GET(DirQ,QNdx)
             DirQ:Name=LOWER(DirQ:Name)
             LenFN=LEN(CLIP(DirQ:Name))
             Exten=SUB(DirQ:Name,LenFn-3,4) ; DirQ:shortname=Exten
             PUT(DirQ) 

         IF BAND(DirQ:Attrib,FF_:Directory) OR DirQ:Name='.' OR DirQ:Name='..'   
            DELETE(DirQ)  !skip . or .. Dirs

         ELSIF DirQ:Date > DateCutoff THEN
            !DirQ:Name=' date ' & DirQ:Name ; PUT(DirQ)     !see Date cutoff in list 
            DELETE(DirQ)

         ELSE
             CASE Exten
             OF '.wmf'  !Delete  Zero Byte .WMF OR 12345678.WMF Created by CPCS
                    IF ~NUMERIC(DirQ:Name[1:8]) AND DirQ:Size THEN
                        DirQ:Name=' skip ' & DirQ:Name              !Only Delete CPCS or Zero Byte
                    END
             OF '.tmp'
                    IF DirQ:Name[1:3]='pdf' OR DirQ:Name[1:3]='cla' THEN    !Tracker PDF####.Tmp or CLA###.Tmp Temp Report
                    ELSE    
                       DirQ:Name=' skip ' & DirQ:Name
                    END 
             END             
             IF ~DirQ:Name[1] THEN   !is [1] Blank ' skip'  !Change to [2] to test other files
                !    PUT(DirQ)               !Test so Skip in List
                DELETE(DirQ)
             ELSE
                PUT(DirQ)
             END
         END
    END !LOOP Delete files
    SortNow=1
    SortNowWho=WHO(DirQ,SortNow) 
    SORT(DirQ , DirQ:Name)  !  DirQ:Name , DirQ:ShortName , DirQ:Date , DirQ:Time , DirQ:Size , DirQ:Attrib )    

    ProgRecords=RECORDS(DirQ)
    ProgRatio=100/ProgRecords

    !===== Test Window to View Directory() ========================================
    SYSTEM{PROP:PropVScroll}=1
    OPEN(FilesWindow)
    ?ListFiles{PROP:Alrt,255}=MouseLeft
    
    IF IsVIEW THEN 
       UNHIDE(?GoBtn)
    ELSE
       0{PROP:Timer}=1
    END
    ?ListFiles{PROP:PropVScroll}=1
    0{PROP:Text}=0{PROP:Text} &'  '& WinTempBS & ' (' & RECORDS(DirQ) &' records) ' 

    IF YES_Delete_Files=0 THEN 
        0{PROP:Text}='0=YES_Delete - ' & 0{PROP:Text}
        ?ListFiles{PROP:Background}=80000018h   !Color if Testing
        ?ListFiles{PROP:Tip}='Files with NOT really be deleted'
    END

    ACCEPT
       CASE ACCEPTED()
       OF ?GoBtn ; HIDE(?) ; 0{PROP:Timer}=5
       END
       CASE EVENT()
       OF EVENT:Timer 
          LOOP 16 TIMES
                GET(DIRQ,ProgNdx+1) ; IF ERRORCODE() THEN 0{PRop:Timer}=0 ; BREAK. 
                ProgNdx+=1
                ?ListFiles{PROP:Selected}=ProgNdx
                cFileName=WinTempBS & clip(DirQ:Name) 

                IF 0 = YES_Delete_Files THEN
                    DB('Did NOT DeleteFile: ' & DirQ:Name)
                    DirQ:ShortName='0=YES Delete' ; PUT(DIRQ) ; CYCLE
                    CYCLE 
                END 

                IF ~DeleteFile(cFileName) THEN 
                    DirQ:ShortName='Error ' & GetLastError() ; PUT(DIRQ)
                    DISPLAY ; BREAK
                ELSE
                    !Delete worked !DirQ:ShortName='' ; DirQ:Time=0 ; PUT(DIRQ)
                END 
                DB('Deleted: ' & DirQ:Name)
          END                 
          ProgPctNew = ProgNdx * ProgRatio
          IF ProgPctNew > ProgPct THEN ProgPct=ProgPctNew ; DISPLAY(?ProgPct).
          IF 0{PRop:Timer}=0 THEN 
             IF ~IsWAIT AND ~IsView THEN POST(EVENT:CloseWindow).
          END
       END 
       CASE FIELD()
       OF ?ListFiles
          CASE EVENT() !Click Header to Sort, Right Click Reverses
          OF EVENT:NewSelection !DblClick to View 1 File
             IF KEYCODE()=MouseLeft2 THEN
                GET(DirQ,CHOICE(?ListFiles))
                MESSAGE(CLIP(DirQ:Name) &'|'& DirQ:ShortName &'|'& DirQ:Date &'|'& DirQ:Time &'|'& DirQ:Size |
                        &'|'& DirQ:Attrib,'File '&CHOICE(?ListFiles),,,,2)
             END
          OF EVENT:PreAlertKey
             IF KEYCODE()=CtrlC THEN
                Dir2Clp='Name<9>Short<9>Date<9>Time<9>Size<9>Attr'
                LOOP QNdx = 1 TO RECORDS(DirQ)
                   GET(DirQ,QNdx)
                   Dir2Clp=Dir2Clp & CLIP('<13,10>' & |
                           CLIP(DirQ:Name) &'<9>'& DirQ:ShortName &'<9>'& FORMAT(DirQ:Date,@d02) &'<9>'& FORMAT(DirQ:Time,@t04) |
                           &'<9>'& DirQ:Size &'<9>'& DirQ:Attrib)
                END !LOOP
                SETCLIPBOARD(Dir2Clp)
             END !IF CtrlC
             IF ?ListFiles{PROPList:MouseDownRow} > 0 THEN CYCLE.
             IF ?ListFiles{PROPList:MouseDownZone} = LISTZONE:right THEN CYCLE.
          OF EVENT:AlertKey
             IF ?ListFiles{PROPList:MouseDownZone} = LISTZONE:right THEN CYCLE.
         
             SortNow=?ListFiles{PROPList:MouseDownField}
             IF ?ListFiles{PROPList:MouseDownRow} = 0  AND SortNow THEN
                IF SortNow<>ABS(SortLast) THEN SortLastWho=',' & SortNowWho.
                SortNowWho=CHOOSE(SortNow=SortLast,'-','+') & WHO(DirQ,SortNow)
                0{PROP:Text}='Directory SORT (' & SortNow &' /'& SortLast &') ' & CLIP(SortNowWho) & SortLastWho
                SORT(DirQ,CLIP(SortNowWho) & SortLastWho)
                SortLast = CHOOSE(SortNow=ABS(SortLast),-1*SortLast,SortNow)
                DISPLAY
             END
          END ! Case EVENT()
       END ! Case FIELD()
    END !ACCEPT
    CLOSE(FilesWindow)
!------------------------------
DB   PROCEDURE(STRING xMessage)
Prfx EQUATE('TmpCln: ')
sz   CSTRING(SIZE(Prfx)+SIZE(xMessage)+1),AUTO
  CODE 
  sz  = Prfx & CLIP(xMessage)
  OutputDebugString( sz )