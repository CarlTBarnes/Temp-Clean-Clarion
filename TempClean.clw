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

YES_Delete_Files   EQUATE(0)    !<--- When (1) DeleteFile() actually happens, else outputs debug only
YES_Remove_Folders EQUATE(1)    !<--- When (1) RemoveDirectory() that are Empty folders, requires YES_Delete_Files=1 to really happen
File_Cutoff_Days   EQUATE(4)    !<--- Files   MUST be 4 days  old to be Deleted, change to (0) for all files
Folder_Cutoff_Days EQUATE(10)   !<--- Folders MUST be 10 days old to be Removed
                   !^^^^^^^^
                   
! [ ] Test it out before you set YES_Delete as (1)
! [ ] Suggest you put your Company Name in the Window Caption.
                  
                  
! Run with /VIEW commandline to see the files before Cleaning and have to press PURGE button. CW Project is set this way.
!
! CAUTION: This does DELETE files from the TEMP folder. You MUST verify it works as desired or you RISK losing files

! What does this Delete?
!   1. *.Wmf        files zero bytes in size
!   2. [0-9]*.Wmf   files named 12345678*.WMF   CPCS creates WMF with all numeric names that are not always purged.
!   3. CLA*.Tmp     Report preview WMF made by RTL
!   3a. CLA*.Wmf    Report preview WMF made by RTL. In 11.1.13744 (first release) extension changed .TMP to .WMF. Also 13758.  09/17/21
!   4. PDF*.Tmp     Tracker makes these files  
!   5. Empty Folders   Various programs leave folders without files  !10/23/20

!
! Files must be over 4 days old to be purged, in case user has left report open.

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
!            ! Add /VIEW to see files before and have PURGE button. Add /WAIT to pause at end. Do this if BackDoor user.
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
        RemoveDirectory(*CSTRING lpPathName ),BOOL,PROC,RAW,PASCAL,DLL(1),NAME('RemoveDirectoryA')
    END          
  END
IsVIEW  BYTE !/View = Pause to view files. Press PURGE button to run. Also waits at end to see results.
IsWAIT  BYTE !/Wait = Purge automatically, but do NOT close at end so can see results.
  CODE
  IF INSTRING('view',lower(command('')),1) THEN IsVIEW=1. !Run with /VIEW sets IsVIEW=1 
  IF INSTRING('wait',lower(command('')),1) THEN IsWAIT=1. !Run with /WAIT sets IsWAIT=1

  !Uncomment to require Secret command line parameter to run this so users cannot just double click on it. Should not be needed
  !IF ~IsVIEW AND ~IsWAIT AND ~INSTRING('secret',lower(command('')),1) THEN  
  !    Message('Temp Clean can only be run by Company Name software.','Temp Folder Clean')
  !    RETURN  
  !END   
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

FilesWindow WINDOW('Temp Folder Cleanup'),AT(,,320,130),CENTER,GRAY,SYSTEM,FONT('Segoe UI',9,,FONT:regular), |
            RESIZE
        BUTTON('&Purge'),AT(2,1,29,12),USE(?GoBtn),HIDE,TIP('Purge Temp Folder Files - IsVIEW=1')
        BUTTON('&Refresh'),AT(289,1,29,12),USE(?RefreshBtn),KEY(F5Key),HIDE,TIP('F5 to Refresh')
        PROGRESS,AT(39,2,242,9),USE(ProgPct),RANGE(0,100)
        LIST,AT(1,15),FULL,USE(?ListFiles),VSCROLL,TIP('Click heads to sort, Right Click to reverse,' & |
                ' Ctrl+C to Copy'),FROM(DirQ),FORMAT('138L(1)|M~Name~@s255@52L(1)|M~EXT~@s13@40R(1)|' & |
                'M~Date~C(0)@d1@40R(1)|M~Time~C(0)@T4b@46R(1)~Size~C(0)@n13@'),ALRT(CtrlC)
    END
Dir2Clp     ANY
DateCutoff  LONG
LenFN       USHORT
Exten       STRING(4) 
    CODE
    IF ~GetTempPath(SIZE(WinTempBS),WinTempBS) |
    OR ~WinTempBS OR ~EXISTS(WinTempBS)        THEN 
        DB('TempClean: GetTempPath() failed or folder does not exist, WinTempBS="'& WinTempBS &'" Last Error: ' & GetLastError()&' - '& COMMAND('0')) 
        IF IsVIEW THEN
           Message('GetTempPath() failed or folder does not exist||WinTempBS: '& WinTempBS &'||Last Error: ' & GetLastError() &|
                   '||Program: '&COMMAND('0') ,'TempClean:',ICON:Exclamation)
        END 
        RETURN        
    END    
    DateCutoff=TODAY()-File_Cutoff_Days  !comment to see all files or change Equate at Top
    DB('TempFolder: ' & WinTempBS & '  YES_Delete_Files=' & YES_Delete_Files )
    DB('DateCutoff: ' & FORMAT(DateCutoff,@d2) &' Days=' & File_Cutoff_Days) 

    !===== Test Window to View Directory() ========================================
    SYSTEM{PROP:PropVScroll}=1
    OPEN(FilesWindow)
    DO LoadTempFolderToDirQRtn
    
    ?ListFiles{PROP:Alrt,255}=MouseLeft
    IF IsVIEW THEN 
       UNHIDE(?GoBtn) ; UNHIDE(?RefreshBtn)
    ELSE
       0{PROP:Timer}=1
    END
    ?ListFiles{PROP:PropVScroll}=1

    IF YES_Delete_Files=0 THEN 
        0{PROP:Text}='0=YES_Delete - ' & 0{PROP:Text}
        ?ListFiles{PROP:Background}=80000018h   !Color if Testing
        ?ListFiles{PROP:Tip}='Files with NOT really be deleted'
    END

    ACCEPT
       CASE ACCEPTED()
       OF ?GoBtn
          ProgNdx=0 ; ProgPct=0 
          ProgRecords=RECORDS(DirQ)
          ProgRatio=?ProgPct{PROP:RangeHigh} / ProgRecords
          DISABLE(?)
          0{PROP:Timer}=1
       OF ?RefreshBtn ; DO LoadTempFolderToDirQRtn          
       END
       CASE EVENT()
       OF EVENT:Timer 
          LOOP 16 TIMES
                GET(DIRQ,ProgNdx+1) 
                IF ERRORCODE() THEN 
                   0{PROP:Timer}=0
                   IF ~IsWAIT AND ~IsView THEN POST(EVENT:CloseWindow).
                   ProgNdx -= 1
                   BREAK
                END
                ProgNdx+=1
                cFileName=WinTempBS & clip(DirQ:Name) 

                IF 0 = YES_Delete_Files THEN
                    DB('Did NOT DeleteFile: ' & DirQ:Name)
                    DirQ:ShortName='0=YES Delete' ; PUT(DIRQ) ; CYCLE
                    CYCLE 
                END 

                IF BAND(DirQ.Attrib,ff_:DIRECTORY) THEN  !10/23/20  Empty Folders
                   IF ~YES_Remove_Folders THEN 
                       CYCLE 
                   ELSIF ~RemoveDirectory(cFileName) THEN
                       DirQ:ShortName='Error ' & GetLastError() 
                       IF DirQ:ShortName='Error 145' THEN DirQ:ShortName='<Dir> w/Files'.
                       PUT(DIRQ) ; DISPLAY
                   END
                ELSIF ~DeleteFile(cFileName) THEN 
                    DirQ:ShortName='Error ' & GetLastError() ; PUT(DIRQ)
                    DISPLAY ; BREAK
                ELSE
                    !Delete worked !DirQ:ShortName='' ; DirQ:Time=0 ; PUT(DIRQ)
                END 
                DB('Deleted: ' & DirQ:Name)
          END                 
          ?ListFiles{PROP:Selected}=ProgNdx          
          ProgPctNew = ProgNdx * ProgRatio
          IF ProgPctNew > ProgPct THEN ProgPct=ProgPctNew ; DISPLAY(?ProgPct).
       END 

       CASE FIELD()
       OF ?ListFiles
          CASE EVENT() !Click Header to Sort, Right Click Reverses
          OF EVENT:NewSelection !DblClick to View 1 File
             IF KEYCODE()=MouseLeft2 THEN
                GET(DirQ,CHOICE(?ListFiles))
                CASE MESSAGE(DirQ:Name &'||Exten: <9>'& DirQ:ShortName &|
                        '|Date: <9>'& FORMAT(DirQ:Date,@d02) &'|Time: <9>'& FORMAT(DirQ:Time,@t6) &|
                        '|Size: <9>'& DirQ:Size &'|Attrib: <9>'& DirQ:Attrib,|
                        'File '&CHOICE(?ListFiles),,'Close|Explore',,MSGMODE:CANCOPY)
                OF 2 ; RUN('Explorer.exe /select,"' & CLIP(WinTempBS & clip(DirQ:Name)) &'"')
                END
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
    RETURN
!------------------------------
LoadTempFolderToDirQRtn ROUTINE
    FREE(DirQ) ; ProgPct=0                                     
    0{PROP:Text}='Loading Temp Folder ...' ; DISPLAY
    DIRECTORY(DirQ,WinTempBS&'*.tmp',ff_:NORMAL)
    DIRECTORY(DirQ,WinTempBS&'*.wmf',ff_:NORMAL)

    LOOP QNdx = RECORDS(DirQ) TO 1 BY -1
         GET(DirQ,QNdx)
             DirQ:Name=LOWER(DirQ:Name)
             LenFN=LEN(CLIP(DirQ:Name))
             Exten=SUB(DirQ:Name,LenFn-3,4) ; DirQ:shortname=Exten

         IF BAND(DirQ:Attrib,FF_:Directory) OR DirQ:Name='.' OR DirQ:Name='..'   
            DELETE(DirQ)  !skip . or .. Dirs

         ELSIF DirQ:Date > DateCutoff THEN
            !DirQ:Name=' date ' & DirQ:Name ; PUT(DirQ)     !see Date cutoff in list 
            DELETE(DirQ)

         ELSE
             CASE Exten
             OF '.wmf'  !Delete  Zero Byte .WMF OR 12345678.WMF Created by CPCS 
                    IF DirQ:Name[1:3]='cla' THEN        !09/17/21
                       !Delete CLA*.Wmf - RTL changed in 11.1.13744 was .TMP since 1.0
                    ELSIF ~NUMERIC(DirQ:Name[1:8]) AND DirQ:Size THEN
                        DirQ:Name=' skip ' & DirQ:Name              !Only Delete CPCS NUmeric.WMF or Zero Byte
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
    DO AddFoldersToDirQRtn     !10/23/20 
    0{PROP:Text}='Temp Folder Cleanup  '& WinTempBS & '  (' & RECORDS(DirQ) &' records) '     
    SortNow=1
    SortNowWho=WHO(DirQ,SortNow) 
    SORT(DirQ , DirQ:Name)  !  DirQ:Name , DirQ:ShortName , DirQ:Date , DirQ:Time , DirQ:Size , DirQ:Attrib )
    DISPLAY
    EXIT
!------------------------------
AddFoldersToDirQRtn ROUTINE  !10/23/20 seeing a lot of Empty folders so try to remove them. 
!This is NOT a Clarion created folder. Since I am cleaning up my user's temp I decided an empty folder over 10 days old can go.
    DATA
FoldQ    QUEUE(FILE:Queue),PRE(FoldQ) . ! FoldQ:Name  FoldQ:ShortName(8.3?)  FoldQ:Date  FoldQ:Time  FoldQ:Size  FoldQ:Attrib
FoldCutoff LONG
    CODE
    IF ~YES_Remove_Folders THEN EXIT.
    FoldCutoff=TODAY() - Folder_Cutoff_Days
    DIRECTORY(FoldQ,WinTempBS&'*.*',ff_:NORMAL+FF_:Directory)
    LOOP QNdx = RECORDS(FoldQ) TO 1 BY -1
         GET(FoldQ,QNdx)
         IF ~BAND(FoldQ:Attrib,FF_:Directory) OR FoldQ:Name='.' OR FoldQ:Name='..'   !. or .. Dirs
            CYCLE 
         ELSIF FoldQ:Date >= FoldCutoff THEN
            CYCLE 
         END 
         FoldQ:Name=LOWER(FoldQ:Name)
         FoldQ:shortname='<<Dir>'   
         DirQ = FoldQ 
         ADD(DirQ) 
    END 
    EXIT    
!------------------------------
DB   PROCEDURE(STRING xMessage)
Prfx EQUATE('TmpCln: ')
sz   CSTRING(SIZE(Prfx)+SIZE(xMessage)+1),AUTO
  CODE 
  sz  = Prfx & CLIP(xMessage)
  OutputDebugString( sz )