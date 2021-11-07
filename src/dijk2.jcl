//Z85565J JOB 1,NOTIFY=&SYSUID
//**********************************/
//*    dijkstra list example       */
//**********************************/
//***********************************/
//* defs                            */
//***********************************/
//         EXPORT SYMLIST=(SYSUID)
//***********************************/
//* delete dsts                     */
//***********************************/
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   *,SYMBOLS=JCLONLY
    DELETE &SYSUID..DATA.DIJK.OUT PURGE
    SET MAXCC = 0
/*
//***********************************/
//* compile                         */
//***********************************/
//DIJKCBL    EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(DIJK),DISP=SHR
//COBOL.SYSLIB DD DSN=&SYSUID..CBL,DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(DIJK),DISP=SHR
//***********************************/
//************* EXEC ****************/
//EXECCBL IF RC = 0 THEN
//***********************************/
//*RUN       EXEC PGM=DIJK,PARM=('/DEBUG')
//RUN       EXEC PGM=DIJK
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//SYSIN     DD *
a
e
/*
//INFILE    DD *
[nodes]
0001,a
0002,b
0003,c
0004,d
0005,e
[edges]
0001,0002,100
0001,0004,050
0004,0002,100
0004,0005,250
0002,0003,100
0002,0005,250
0003,0005,050
[end]
/*
//OUTFILE   DD DSN=&SYSUID..DATA.DIJK.OUT,
//          DISP=(,CATLG,DELETE),
//          UNIT=SYSDA,
//          LRECL=80,RECFM=FB,
//          SPACE=(80,(1,1),RLSE)
//FAILCBL ELSE
//ENDCBL  ENDIF
