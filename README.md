# Temp Clean Clarion

Clarion Reports generate WMF files named CLA###.TMP (.WMF in 11.1) in the current User Windows Temp folder.
 A crash, bad code or a process shutdown can leave TMP files behind. A Temp folder full of these can cause problems with preview.

I see CPCS report templates create ########.WMF files but not delete them. I see Tracker PDF*.TMP files also being left in the temp folder. This tool also purges these files.

In my Frame shutdown I RUN this Temp Clean EXE every 14 days for each user. It has proved quite useful. At many existing sites I was seeing 1000s of files being purged. It displays the below window with a Progress Bar. 

For this program to really delete files you must change this line from (0) to (1):

```clarion
YES_Delete_Files   EQUATE(0)
```

![capture](readme_cap.png)

You can run with a /VIEW switch and the window will pause with a PURGE button that must be pressed to delete the files.

![capture](readme_view.png)

October 23, 2020 - enhanced to remove empty folders. This has nothing to do with Clarion. I noticed empty folders left in my temp folder and decided to enhance this housekeeping program to remnove them.
 This relies on RemoveDirectory() being documented to only remove empty folders or it throws an error 145.

September 17, 2021 - Changed for Clarion 11.1 release 13744 to also purge Cla*.WMF files. All prior releases created Cla*.TMP. I assume this was done for Any Screen and will continue as it is also in the second release 13758.
