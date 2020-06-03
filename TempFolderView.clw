!Quick way to see contents of Temp Folder
  PROGRAM
  INCLUDE 'KEYCODES.CLW'
  MAP
TempPurgeTmp    PROCEDURE()  
    MODULE('Win32')
        GetTempPath(LONG nBufferLength,*CSTRING lpTempPath),LONG,PASCAL,DLL(1),RAW,NAME('GetTempPathA'),PROC
    END          
  END

  CODE
  TempPurgeTmp()
!-----------
TempPurgeTmp    PROCEDURE()
WinTempBS  CSTRING(256)
QNdx    LONG,AUTO
DirQ    QUEUE(FILE:Queue),PRE(DirQ)
        END ! DirQ:Name  DirQ:ShortName(8.3?)  DirQ:Date  DirQ:Time  DirQ:Size  DirQ:Attrib
    CODE
    IF ~GetTempPath(SIZE(WinTempBS),WinTempBS) THEN RETURN.
    DO LoadFilesRtn 
!--------
LoadFilesRtn    ROUTINE
    DATA
!QNdx    LONG,AUTO
!DirQ    QUEUE(FILE:Queue),PRE(DirQ)
!        END ! DirQ:Name  DirQ:ShortName(8.3?)  DirQ:Date  DirQ:Time  DirQ:Size  DirQ:Attrib
SortNow     SHORT(1)
SortLast    SHORT(1)
SortNowWho  STRING(128)
SortLastWho STRING(128)
_CompileDirTestWnd_ EQUATE(1)

FilesWindow WINDOW('Directory'),AT(,,343,250),FONT('Segoe UI',9,,FONT:regular),SYSTEM,GRAY,RESIZE,ICON(ICON:Pick),CENTER
              LIST,AT(1,1),USE(?ListFiles),FULL,VSCROLL,ALRT(MouseLeft),ALRT(CtrlC),TIP('Click heads to sort, Right Click to reverse, Ctrl+C to Copy'), |
                FORMAT('120L(1)|M~Name~@s255@52L(1)|M~Short Name~@s13@40R(1)|M~Date~C(0)@d1@40R(1)|M~Time~C(0)@T4@56R(1)|M~Size~C(0)@n13@16L(1)|M~Attr~@n3@') ,|
                FROM(DirQ) !, #FIELDS(DirQ:name,DirQ:shortname,DirQ:date,DirQ:time,DirQ:size,DirQ:attrib)
            END
Dir2Clp     ANY

    CODE
    FREE(DirQ)
    IF 1 THEN  
       DIRECTORY(DirQ,WinTempBS&'*.*',ff_:NORMAL)
    ELSE 
       DIRECTORY(DirQ,WinTempBS&'*.tmp',ff_:NORMAL)
       DIRECTORY(DirQ,WinTempBS&'*.wmf',ff_:NORMAL)
    END 

    !--Keep directories, Delete files, Case Name
    LOOP QNdx = RECORDS(DirQ) TO 1 BY -1
         GET(DirQ,QNdx)
         IF BAND(DirQ:Attrib,FF_:Directory) OR DirQ:Name='.' OR DirQ:Name='..'   !. or .. Dirs
            DELETE(DirQ)
         ELSE
             DirQ:Name=UPPER(DirQ:Name[1]) & DirQ:Name[2 : SIZE(DirQ:Name) ]
             PUT(DirQ)
         END
    END 
    SortNow=1
    SortNowWho=WHO(DirQ,SortNow) 
    SORT(DirQ , SortNowWho)  !   , DirQ:ShortName , DirQ:Date , DirQ:Time , DirQ:Size , DirQ:Attrib )    
    
    !===== Test Window to View Directory() ========================================
    SYSTEM{PROP:PropVScroll}=1   
    OPEN(FilesWindow)
    ?ListFiles{PROP:PropVScroll}=1
    0{PROP:Text}=CLIP(0{PROP:Text}) &'  '& WinTempBS & ' (' & RECORDS(DirQ) &' records) '
    ACCEPT
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
    EXIT
