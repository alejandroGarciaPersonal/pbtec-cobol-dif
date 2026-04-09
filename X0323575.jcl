//J0323575 JOB (19),'C.ABRIL   ',CLASS=U,MSGCLASS=N,
//           REGION=0M,COND=(0,NE)
//*
//**********************************************************************
//*  RESERVADA: 06/08/2024  RF01871974
//* IMPLANTADA: 27/09/2024  RF01908236
//* MODIFICADA: 30/09/2024  RF01909316y RF01909538
//* MODIFICADA: 24/10/2024  RF01930975
//* MODIFICADA: 21/11/2024  RF01953586
//* MODIFICADA: 18/12/2024  RF01973581
//* MODIFICADA: 30/01/2025  RF02000804
//* MODIFICADA: 25/02/2025  RF02022093
//* MODIFICADA: 18/03/2025  RF02039984
//* MODIFICADA: 23/03/2025  RF02327259
//**********************************************************************
//*
//*               PARTICIPACION BENEFICIOS
//*
//*            GENERACION FICHERO UOA Y TOTALES
//*
//*
//**     FECHA DE EJECUCION :
//**     %%ODAY/%%OMONTH/%%OYEAR
//**
//* %%LIBSYM  EXPLO.CTM.SYSIN    %%MEMSYM  FINMES
//* %%SET %%F = %%CALCDATE %%ODATE -25
//* %%SET %%A = %%SUBSTR %%F 1 2
//* %%SET %%M = %%SUBSTR %%F 3 2
//* %%INCLIB EXPLO.CTM.SYSIN %%INCMEM @A
//* %%INCLIB EXPLO.CTM.SYSIN %%INCMEM @YEAR
//* %%SET %%F1 = %%@A.%%MMDD_%%M
//*
/*JOBPARM SYSAFF=%%PARTR
//*
//*====================================================================*
//  INCLUDE MEMBER=JRCOBDB2
//**********************************************************************
//PASODB01 EXEC R002TBGD,ACT='L'
//**********************************************************************
//* R002TBGD,ACT='L' - COMPRUEBA QUE ESTE EN LECTURA BD002TBG
//**********************************************************************
//PASODB02 EXEC R340GBTD,ACT='L'
//**********************************************************************
//* R340GBTD,ACT='L' - COMPRUEBA QUE ESTE EN LECTURA BD340GBT
//**********************************************************************
//PASODB03 EXEC R520GSCD,ACT='L'
//**********************************************************************
//* R520GSCD,ACT='L' - COMPRUEBA QUE ESTE EN LECTURA BD520GSC
//**********************************************************************
//PASODB04 EXEC R350PBFD,ACT='L'
//**********************************************************************
//* R350PBFD,ACT='L' - COMPRUEBA QUE ESTE EN LECTURA BD350PBF
//**********************************************************************
//PASODB05 EXEC R540GCCD,ACT='L'
//**********************************************************************
//* R540GCCD,ACT='L' - COMPRUEBA QUE ESTE EN LECTURA BD540GCC
//**********************************************************************
//**********************************************************************
//BORRAINI EXEC INIBORRA
//**********************************************************************
//ENTRADA  DD *
  LISTCAT LEVEL(EXPLO.P0323575)
//*
//**********************************************************************
//PASO010  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTIN   DD DSN=EXPLO.P0323573.PASO070.SYS003.MOD3000.AMB,
//            DISP=SHR
//SORTOUT  DD DSN=EXPLO.P0323575.B540JP99.MOD3000.SORT,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(5000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=108,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 SORT FIELDS=(1,11,CH,A)
 SUM FIELDS=NONE
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//**********************************************************************
//PASO020  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.B540JP99.MOD3000.SORT,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323567.S9PASO75.CONRAMO.SORT,
//            DISP=SHR
//SORTOUT  DD DSN=EXPLO.P0323575.PASO020.JOIN,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(5000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=164,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,11,A)
 JOINKEYS FILE=F2,FIELDS=(15,11,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,108,F2:1,108,?)
 SORT FIELDS=COPY
 SUM FIELDS=NONE
 OUTFIL FNAMES=SORTOUT,INCLUDE=(217,1,CH,EQ,C'B'),
 BUILD=(1,86,144,6,134,7,C'N',C'00',171,5,161,8,147,3,113,2,C'  ',
        161,8,150,1,C' ',203,6,209,1,C'  ',210,7,C'       ',
        185,8,C' ')
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//**********************************************************************
//PASO030  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.PASO020.JOIN,DISP=SHR
//SORTOUT  DD DSN=EXPLO.P0323575.PASO030.JOIN,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=164,BLKSIZE=0),UNIT=(SYSDA,3)
 SORT FIELDS=(1,164,CH,A)
 SUM FIELDS=NONE
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//**********************************************************************
//PASO040  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.PASO030.JOIN,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323567.CARTERA.TECNICA.SORTOUT,
//            DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.PASO040.JOIN.SORTA,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(5000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=164,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTOUT2 DD DSN=EXPLO.P0323575.PASO040.JOIN.SORT1,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(5000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=164,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,11,A)
 JOINKEYS FILE=F2,FIELDS=(14,11,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,164,F2:1,800,?)
 SORT FIELDS=COPY
 SUM FIELDS=NONE
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(965,1,CH,EQ,C'B'),
 BUILD=(1,42,917,2,45,55,216,1,224,7,239,8,168,3,919,2,408,2,370,8,
        359,1,385,1,433,6,326,10,439,7,231,8,740,1)
 OUTFIL FNAMES=SORTOUT2,INCLUDE=(965,1,CH,EQ,C'1'),
 BUILD=(1,164)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//**********************************************************************
//PASO050  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.PASO040.JOIN.SORT1,
//            DISP=SHR
//SORTOUT  DD DSN=EXPLO.P0323575.PASO050.JOIN.SORT2,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=164,BLKSIZE=0),UNIT=(SYSDA,3)
 SORT FIELDS=COPY
 SUM FIELDS=NONE
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//**********************************************************************
//PASO060  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.PASO050.JOIN.SORT2,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323567.CARTERA.TECNICA.SORTOUT,
//            DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.PASO060.JOIN.SORT3,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(5000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=164,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,7,A)
 JOINKEYS FILE=F2,FIELDS=(14,7,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,164,F2:1,800,?)
 SORT FIELDS=COPY
 SUM FIELDS=NONE
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(965,1,CH,EQ,C'B'),
 BUILD=(1,42,917,2,45,55,216,1,224,7,239,8,168,3,919,2,408,2,370,8,
        359,1,385,1,433,6,326,10,439,7,231,8,740,1)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//**********************************************************************
//PASO070  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.PASO060.JOIN.SORT3,
//            DISP=SHR
//SORTOUT  DD DSN=EXPLO.P0323575.PASO070.JOIN.SORTPC,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=164,BLKSIZE=0),UNIT=(SYSDA,3)
 SORT FIELDS=(1,11,CH,A)
 SUM FIELDS=NONE
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//**********************************************************************
//PASO080  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.PASO070.JOIN.SORTPC,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323567.S2PASO20.SORTOUT,
//***      FICOL LREC=1264
//            DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.PASO080.JOIN.PCOK,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(5000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=168,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,11,A)
 JOINKEYS FILE=F2,FIELDS=(2,11,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,164,F2:1,1230,?)
 SORT FIELDS=COPY
 SUM FIELDS=NONE
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(1395,1,CH,EQ,C'B'),
 BUILD=(1,148,356,12,156,8)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*********************************************************************
//PASO090  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.PASO080.JOIN.PCOK,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.PASO090.JOIN.PCOK,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=164,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
  OUTREC FIELDS=(1,148,149,12,ZD,TO=PD,LENGTH=7,160,8)
/*
//**********************************************************************
//PASO100  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.PASO090.JOIN.PCOK,
//            DISP=SHR
//         DD DSN=EXPLO.P0323575.PASO040.JOIN.SORTA,
//            DISP=SHR
//SORTOUT  DD DSN=EXPLO.P0323575.PASO100.JOIN.SORT4,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=164,BLKSIZE=0),UNIT=(SYSDA,3)
 SORT FIELDS=(1,11,CH,A)
 SUM FIELDS=NONE
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//**********************************************************************
//PASO110  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323567.S2PASO20.SORTOUT,
//***      FICOL LREC=1264
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323575.PASO100.JOIN.SORT4,
//            DISP=SHR
//SORTAMB  DD DSN=EXPLO.P0323575.CTPB.EXT.AMB,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=177,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTF1   DD DSN=EXPLO.P0323575.CTPB.EXT.F1,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=164,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(2,11,A)
 JOINKEYS FILE=F2,FIELDS=(1,11,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,1230,F2:1,164,?)
 SORT FIELDS=COPY
 SUM FIELDS=NONE
 OUTFIL FNAMES=SORTAMB,INCLUDE=(1395,1,CH,EQ,C'B'),
 BUILD=(1231,164,238,3,141,2,287,1,170,7)
 OUTFIL FNAMES=SORTF1,INCLUDE=(1395,1,CH,EQ,C'1'),
 BUILD=(1232,164)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*********************************************************************
//PASO120  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CTPB.EXT.AMB,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CTPB.EXT.AMB.GP0,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=177,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
  INCLUDE COND=(165,3,CH,EQ,C'000',OR,
                165,3,CH,EQ,C'001',OR,
                165,3,CH,EQ,C'009')
  OUTREC FIELDS=(1,164,C'   ',168,10)
/*
//**********************************************************************
//PASO130  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CTPB.EXT.AMB,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323575.CTPB.EXT.AMB.GP0,
//            DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.PASO130.JOIN,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(5000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=177,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTOUT2 DD DSN=EXPLO.P0323575.PASO130.JOIN2,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(5000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=177,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTOUT3 DD DSN=EXPLO.P0323575.PASO130.JOIN3,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(5000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=177,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,11,A)
 JOINKEYS FILE=F2,FIELDS=(1,11,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,177,F2:1,177,?)
 SORT FIELDS=COPY
 SUM FIELDS=NONE
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(355,1,CH,EQ,C'B'),
 BUILD=(178,177)
 OUTFIL FNAMES=SORTOUT2,INCLUDE=(355,1,CH,EQ,C'1'),
 BUILD=(1,177)
 OUTFIL FNAMES=SORTOUT3,INCLUDE=(355,1,CH,EQ,C'2'),
 BUILD=(178,177)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//**********************************************************************
//PASO140  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.PASO130.JOIN,
//            DISP=SHR
//         DD DSN=EXPLO.P0323575.PASO130.JOIN2,
//            DISP=SHR
//SORTOUT  DD DSN=EXPLO.P0323575.CTPB.EXT.AMB.GPOK,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=177,BLKSIZE=0),UNIT=(SYSDA,3)
 SORT FIELDS=(1,11,CH,A)
 SUM FIELDS=NONE
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*********************************************************************
//PASO150  EXEC PGM=SORT,REGION=0M
//SORTIN   DD DSN=EXPLO.P0323575.CTPB.EXT.AMB.GPOK,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CTPB.EXT.AMB.GPNO0,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=177,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
  INCLUDE COND=(165,3,CH,NE,C'000')
/*
//**********************************************************************
//PASO160  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CTPB.EXT.AMB.GP0,
//            DISP=SHR
//         DD DSN=EXPLO.P0323575.CTPB.EXT.AMB.GPNO0,
//            DISP=SHR
//SORTOUT  DD DSN=EXPLO.P0323575.CTPB.EXT.AMB.GP,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=177,BLKSIZE=0),UNIT=(SYSDA,3)
 SORT FIELDS=(1,11,CH,A)
 SUM FIELDS=NONE
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//**********************************************************************
//PASO180  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CTPB.EXT.AMB.GP,
//            DISP=SHR
//SORTOUT  DD DSN=EXPLO.P0323575.CTPB.EXT.SORT,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=178,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 SORT FIELDS=(1,177,CH,A)
 SUM FIELDS=NONE
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//**********************************************************************
//PASO190  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//***** solo los registros en vigor       ******************************
//**********************************************************************
//SORTIN   DD DSN=EXPLO.P03000AA.S100J003.SORTOUT,
//            DISP=SHR
//SORTOUT  DD DSN=EXPLO.P0323575.S100J003.SORT1,
//            DISP=(NEW,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=154,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
   SORT FIELDS=(1,5,CH,A,6,30,CH,A)
   INCLUDE COND=(1,5,CH,EQ,C'35010',AND,25,4,CH,EQ,C'9999')
   SUM FIELDS=NONE
   END
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//**********************************************************************
//PASO200  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.S100J003.SORT1,DISP=SHR
//*
//SORTJNF2 DD DSN=EXPLO.P0323575.CTPB.EXT.SORT,DISP=SHR
//*
//SOLOF1   DD DSN=EXPLO.P0323575.PASO200.SOLOF1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(2500,950),RLSE),
//            DCB=(LRECL=154,RECFM=FB)
//*
//SOLOF2   DD DSN=EXPLO.P0323575.PASO200.SOLOF2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(2500,950),RLSE),
//            DCB=(LRECL=178,RECFM=FB)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  JOINKEYS FILE=F1,FIELDS=(06,11,A),SORTED
  JOINKEYS FILE=F2,FIELDS=(01,11,A),SORTED
  JOIN UNPAIRED
  REFORMAT FIELDS=(F1:1,154,F2:1,178,?)
  SORT FIELDS=COPY
  OUTFIL FNAMES=SOLOF1,INCLUDE=(333,1,CH,EQ,C'1'),
  BUILD=(1,154)
  OUTFIL FNAMES=SOLOF2,INCLUDE=(333,1,CH,EQ,C'B'),
  BUILD=(155,178)

//*
//*********************************************************************
//PASO210  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.PASO200.SOLOF2,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.PASO210.SOLOF2.S1,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=178,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(01,11,CH,A)
  SUM FIELDS=NONE
/*
//**********************************************************************
//PASO220  EXEC PGM=IKJEFT01,DYNAMNBR=100,REGION=0M
//**********************************************************************
//SYS001   DD DSN=EXPLO.P0323575.S100J003.SORT1,
//            DISP=SHR
//SYS002   DD DSN=EXPLO.P0323575.PASO210.SOLOF2.S1,
//            DISP=SHR
//SYS003   DD DSN=EXPLO.P0323575.GREM.LTA,
//            DISP=(NEW,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=178,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSOUT   DD SYSOUT=*
//SYSDBOUT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SYSTSIN  DD *
 DSN SYSTEM(DSNR)
 RUN PROGRAM(B350J092) PLAN(PLURBAT)
END
/*
//**********************************************************************
//PASO230  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CTPB.EXT.SORT,DISP=SHR
//*
//SORTJNF2 DD DSN=EXPLO.P0323575.GREM.LTA,DISP=SHR
//*
//SOLOF1   DD DSN=EXPLO.P0323575.CTPB.EXT.SORT.F1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(2500,950),RLSE),
//            DCB=(LRECL=178,RECFM=FB)
//SOLOF2   DD DSN=EXPLO.P0323575.CTPB.EXT.SORT.F2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(2500,950),RLSE),
//            DCB=(LRECL=178,RECFM=FB)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  JOINKEYS FILE=F1,FIELDS=(01,11,A),SORTED
  JOINKEYS FILE=F2,FIELDS=(01,11,A),SORTED
  JOIN UNPAIRED,F1,F2
  REFORMAT FIELDS=(F1:1,178,F2:1,178,?)
  SORT FIELDS=COPY
  OUTFIL FNAMES=SOLOF1,INCLUDE=(357,1,CH,EQ,C'B'),
  BUILD=(179,178)
  OUTFIL FNAMES=SOLOF2,INCLUDE=(357,1,CH,EQ,C'1'),
  BUILD=(1,178)
//*
//*********************************************************************
//PASO240  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CTPB.EXT.SORT.F1,DISP=SHR
//         DD DSN=EXPLO.P0323575.CTPB.EXT.SORT.F2,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CTPB.EXT.SORT.UNION,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=178,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(01,11,CH,A)
/*
//**********************************************************************
//PASO250  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTIN   DD DSN=EXPLO.P0323567.CARTERA.TECNICA.SORTOUT,
//            DISP=SHR
//SORTOUT  DD DSN=EXPLO.P0323575.CARTERA.TECNICA.SORTOUT1,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(1300,1300),RLSE),
//            DCB=(RECFM=FB,LRECL=800,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 SORT FIELDS=(14,11,CH,A)
 INCLUDE COND=(1,11,CH,NE,C'           ')
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//**********************************************************************
//PASO260  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CARTERA.TECNICA.SORTOUT1,
//            DISP=SHR
//SORTOUT  DD DSN=EXPLO.P0323575.CARTERA.TECNICA.ACUM,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(1300,1300),RLSE),
//            DCB=(RECFM=FB,LRECL=800,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 SORT FIELDS=(14,11,CH,A)
 INCLUDE COND=(184,1,CH,NE,C'A')
 SUM FIELDS=(275,7,PD)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//**********************************************************************
//PASO270  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CTPB.EXT.SORT.UNION,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323575.CARTERA.TECNICA.ACUM,
//            DISP=SHR
//SORTAMB  DD DSN=EXPLO.P0323575.CTPB.EXT.SORT1,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=178,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTF1   DD DSN=EXPLO.P0323575.CTPB.EXT.NOK,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=178,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,11,A)
 JOINKEYS FILE=F2,FIELDS=(14,11,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,178,F2:1,800,?)
 SORT FIELDS=COPY
 SUM FIELDS=NONE
 OUTFIL FNAMES=SORTAMB,INCLUDE=(979,1,CH,EQ,C'B'),
 BUILD=(1,148,453,7,156,23)
 OUTFIL FNAMES=SORTF1,INCLUDE=(979,1,CH,EQ,C'1'),
 BUILD=(1,178)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//**********************************************************************
//PASO280  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CTPB.EXT.SORT1,
//            DISP=SHR
//         DD DSN=EXPLO.P0323575.CTPB.EXT.NOK,
//            DISP=SHR
//SORTOUT  DD DSN=EXPLO.P0323575.CTPB.EXT.SORT2,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=178,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 SORT FIELDS=(1,33,CH,A)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//**********************************************************************
//PASO290  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CTPB.EXT.SORT2,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323574.B350JI83.SORTOUT,
//            DISP=SHR
//SORTOUT  DD DSN=EXPLO.P0323575.CTPB.EXT.SORT3,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(5000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=194,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,11,A)
 JOINKEYS FILE=F2,FIELDS=(1,11,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,178,F2:1,108,?)
 SORT FIELDS=COPY
 SUM FIELDS=NONE
 OUTFIL FNAMES=SORTOUT,INCLUDE=(287,1,CH,EQ,C'B'),
 BUILD=(1,178,265,16)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//**********************************************************************
//PASO294  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323567.S2PASO20.SORTOUT,
//***      FICOL LREC=1264
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323575.CTPB.EXT.SORT3,
//            DISP=SHR
//SORTOUT  DD DSN=EXPLO.P0323575.PASO294.JOIN,
//***      FICOL LREC=1264
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=1264,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(2,11,A)
 JOINKEYS FILE=F2,FIELDS=(1,11,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,1264,F2:1,194,?)
 SORT FIELDS=COPY
 SUM FIELDS=NONE
 OUTFIL FNAMES=SORTOUT,INCLUDE=(1459,1,CH,EQ,C'B'),
 BUILD=(1,1264)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*
//*********************************************************************
//PASO300  EXEC PGM=IKJEFT01,DYNAMNBR=100,REGION=0M
//*********************************************************************
//SYSTSIN  DD *
 DSN SYSTEM(DSNR)
 RUN PROGRAM(B350JI20) PLAN(PLURBAT)
END
/*
// INCLUDE MEMBER=IDMSDD02
// INCLUDE MEMBER=RCOLVIDA A=N
// INCLUDE MEMBER=RGSCAR   A=N
// INCLUDE MEMBER=RCOSETE  A=N
//*INCLUDE MEMBER=RREDTER  A=N
// INCLUDE MEMBER=RDAGENER A=N
//SYS006   DD DSN=EXPLO.P0323573.B350J028.FICHA,DISP=SHR
//SYS001   DD DSN=EXPLO.P0323575.CTPB.EXT.SORT3,DISP=SHR
//SYS002   DD DSN=EXPLO.P0323574.B350J084.SYS003,DISP=SHR
//SYS012   DD DSN=EXPLO.P0323574.B350JX84.SYS003.BONO,DISP=SHR
//SYS013   DD DSN=EXPLO.P0323574.B350JZ84.SYS003,DISP=SHR
//SYS014   DD DSN=EXPLO.P0323575.PASO294.JOIN,DISP=SHR
//***      FICOL LREC=1264
//*
//SYS004   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SJI03,
//            DISP=(,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYS005   DD DSN=EXPLO.P0323575.CASOUS03.FICHINC.INCI,
//            DISP=(,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=800,BLKSIZE=0)
//*
//*YSOUT   DD SYSOUT=*
//SYSOUT   DD DSN=EXPLO.P0323575.CASOUS03.SYSOUT.LT3,
//            DISP=(,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(5000,1500),RLSE),
//            DCB=(RECFM=FB,LRECL=133,BLKSIZE=0)
//*
//SYSDBOUT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//**********************************************************************
//PASO310  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SJI03,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURI,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
  INCLUDE COND=(439,5,CH,EQ,C'PLURI')
/*
//**********************************************************************
//PASO320  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SJI03,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURA,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
  INCLUDE COND=(439,5,CH,EQ,C'PLURA')
/*
//*********************************************************************
//PASO330  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURI,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURI.TOTPOL,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(07,11,CH,A)
  SUM FIELDS=(186,13,ZD,235,18,ZD,253,18,ZD,271,18,ZD,445,18,ZD)
/*
//*********************************************************************
//PASO331  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURI.TOTPOL,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURI.TPOLSP,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
 INCLUDE COND=(146,3,CH,EQ,C'   ')
/*
//*********************************************************************
//PASO332  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURI.TOTPOL,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURI.TPOLGP,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
 INCLUDE COND=(146,3,CH,NE,C'   ')
/*
//*********************************************************************
//PASO340  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURA,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURA.TOTPOL,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(07,11,CH,A)
  SUM FIELDS=(186,13,ZD,235,18,ZD,253,18,ZD,271,18,ZD,445,18,ZD)
/*
//*********************************************************************
//PASO341  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURA.TOTPOL,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURA.TPOLSP,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
 INCLUDE COND=(146,3,CH,EQ,C'   ')
/*
//*********************************************************************
//PASO342  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURA.TOTPOL,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURA.TPOLGP,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
 INCLUDE COND=(146,3,CH,NE,C'   ')
/*
//*********************************************************************
//PASO350  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURI.TPOLGP,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURI.TGP,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(146,3,CH,A)
  SUM FIELDS=(186,13,ZD,235,18,ZD,253,18,ZD,271,18,ZD,445,18,ZD)
/*
//*********************************************************************
//PASO351  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURI.TGP,DISP=SHR
//         DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURI.TPOLSP,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURI.GP,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(146,3,CH,A,7,11,CH,A)
/*
//*********************************************************************
//PASO360  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURA.TPOLGP,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURA.GP,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(146,3,CH,A)
  SUM FIELDS=(186,13,ZD,235,18,ZD,253,18,ZD,271,18,ZD,445,18,ZD)
/*
//*********************************************************************
//PASO370  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SJI03,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONOR,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
  INCLUDE COND=(439,5,CH,EQ,C'BONO ',AND,99,1,CH,EQ,C'S')
/*
//*********************************************************************
//PASO371  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SJI03,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONONR,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
  INCLUDE COND=(439,5,CH,EQ,C'BONO ',AND,99,1,CH,EQ,C'N')
/*
//********************************************************************
//PASO380  EXEC PGM=SORT,REGION=0M
//********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SJI03,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONOA,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
  INCLUDE COND=(439,5,CH,EQ,C'BONOA')
/*
//*********************************************************************
//PASO390  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONOR,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONOR.TOT,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(07,11,CH,A)
  SUM FIELDS=(186,13,ZD,235,18,ZD,253,18,ZD,271,18,ZD,445,18,ZD)
/*
//*********************************************************************
//PASO400  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONOA,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONOA.TOT,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(07,11,CH,A)
  SUM FIELDS=(186,13,ZD,235,18,ZD,253,18,ZD,271,18,ZD,445,18,ZD)
/*
//*********************************************************************
//PASO410  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONOR.TOT,DISP=SHR
//         DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONONR,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONOR.GP,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(146,3,CH,A)
  SUM FIELDS=(186,13,ZD,235,18,ZD,253,18,ZD,271,18,ZD,445,18,ZD)
/*
//*********************************************************************
//PASO411  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONOA.TOT,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONOA.TOTPOL,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
 INCLUDE COND=(146,3,CH,EQ,C'   ')
/*
//*********************************************************************
//PASO412  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONOA.TOT,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONOA.TOTGP,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
 INCLUDE COND=(146,3,CH,NE,C'   ')
/*
//*********************************************************************
//PASO420  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONOA.TOTGP,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONOA.GP,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(146,3,CH,A)
  SUM FIELDS=(186,13,ZD,235,18,ZD,253,18,ZD,271,18,ZD,445,18,ZD)
/*
//*********************************************************************
//PASO430  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SJI03,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTB.SUM,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(07,11,CH,A)
  SUM FIELDS=(235,18,ZD,253,18,ZD,445,18,ZD)
/*
//*********************************************************************
//PASO440  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SJI03,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTB.BONOGP,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
  INCLUDE COND=(439,5,CH,EQ,C'BONO ',AND,99,1,CH,EQ,C'N',AND,
                146,3,CH,NE,C'   ')
/*
//*********************************************************************
//PASO440A EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SJI03,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTB.BONOGPSR,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
  INCLUDE COND=(439,5,CH,EQ,C'BONO ',AND,99,1,CH,EQ,C'S',AND,
                146,3,CH,NE,C'   ')
/*
//*********************************************************************
//PASO441  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SJI03,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTB.BONOSP,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
  INCLUDE COND=(439,5,CH,EQ,C'BONO ',AND,99,1,CH,EQ,C'N',AND,
                146,3,CH,EQ,C'   ')
/*
//*********************************************************************
//PASO450  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTB.BONOGP,DISP=SHR
//         DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTB.BONOGPSR,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTB.BONOGPPR,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(146,3,CH,A)
  SUM FIELDS=(186,13,ZD,235,18,ZD,253,18,ZD,271,18,ZD,445,18,ZD)
/*
//*********************************************************************
//PASO451  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTB.BONOSP,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTB.BONOSPPR,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(7,11,CH,A)
  SUM FIELDS=(186,13,ZD,235,18,ZD,253,18,ZD,271,18,ZD,445,18,ZD)
/*
//*********************************************************************
//PASO452  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTB.BONOGPPR,DISP=SHR
//         DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTB.BONOSPPR,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTB.BONOPR,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(146,3,CH,A,7,11,CH,A)
/*
//*
//**********************************************************************
//PASO460  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTB.SUM,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTB.BONOPR,
//            DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.CASOUS03.FICHLTA.AMB,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTOUT2 DD DSN=EXPLO.P0323575.CASOUS03.FICHLTA.F1,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTOUT3 DD DSN=EXPLO.P0323575.CASOUS03.FICHLTA.F2,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(146,3,A)
 JOINKEYS FILE=F2,FIELDS=(146,3,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,550,F2:1,550,?)
 SORT FIELDS=COPY
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(1101,1,CH,EQ,C'B'),
 BUILD=(1,185,736,13,199,72,821,18,289,262)
 OUTFIL FNAMES=SORTOUT2,INCLUDE=(1101,1,CH,EQ,C'1'),
 BUILD=(1,550)
 OUTFIL FNAMES=SORTOUT3,INCLUDE=(1101,1,CH,EQ,C'2'),
 BUILD=(551,550)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//*********************************************************************
//PASO470  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHLTA.AMB,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTB.SUM.S1,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(146,3,CH,A)
  SUM FIELDS=(235,18,ZD,253,18,ZD,445,18,ZD)
/*
//*********************************************************************
//* DISTINCI�N DE PRIMA                                               *
//*********************************************************************
//PASO480  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SJI03,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINPRU,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(07,11,CH,A,116,8,CH,D)
  INCLUDE COND=(18,15,CH,EQ,C'               ')
  SUM FIELDS=NONE
/*
//*********************************************************************
//* DISTINCI�N DE PRIMA                                               *
//*********************************************************************
//PASO490  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINPRU,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINPRI,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(07,11,CH,A)
  SUM FIELDS=NONE
/*
//*********************************************************************
//* DISTINCI�N DE PRIMA                                               *
//*********************************************************************
//PASO500  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SJI03,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNR,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(07,11,CH,A,108,4,CH,A)
  INCLUDE COND=(18,15,CH,EQ,C'               ')
  SUM FIELDS=NONE
/*
//**********************************************************************
//PASO510  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNR,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINPRI,
//            DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRA,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTOUT2 DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRF,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTOUT3 DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNR3,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(7,11,A,116,8,A)
 JOINKEYS FILE=F2,FIELDS=(7,11,A,116,8,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,550,F2:1,550,?)
 SORT FIELDS=COPY
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(1101,1,CH,EQ,C'B'),
 BUILD=(1,550)
 OUTFIL FNAMES=SORTOUT2,INCLUDE=(1101,1,CH,EQ,C'2'),
 BUILD=(551,550)
 OUTFIL FNAMES=SORTOUT3,INCLUDE=(1101,1,CH,EQ,C'1'),
 BUILD=(1,550)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//*********************************************************************
//* DISTINCI�N DE PRIMA                                               *
//*********************************************************************
//PASO520  EXEC PGM=SORT,REGION=0M
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRF,DISP=SHR
//         DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNR3,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRF1,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(01,17,CH,A)
  INCLUDE COND=(99,1,CH,EQ,C'N')
/*
//*********************************************************************
//* DISTINCI�N DE PRIMA                                               *
//*********************************************************************
//PASO530  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRA,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRA1,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
  INCLUDE COND=(99,1,CH,EQ,C'N')
  SUM FIELDS=NONE
/*
//*********************************************************************
//PASO540  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRA1,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRA2,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(7,11,A,108,8,A),FORMAT=BI
/*
//*********************************************************************
//PASO550  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRA2,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRA3,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(7,11,A),FORMAT=BI
 SUM FIELDS=NONE
/*
//*********************************************************************
//PASO560  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRA1,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRA4,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(7,11,A,108,8,D),FORMAT=BI
/*
//*********************************************************************
//PASO570  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRA4,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRA5,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(7,11,A),FORMAT=BI
 SUM FIELDS=NONE
/*
//*********************************************************************
//* DISTINCI�N DE PRIMA                                               *
//*********************************************************************
//PASO580  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRF1,DISP=SHR
//         DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRA3,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRUN,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
  SUM FIELDS=NONE
/*
//*********************************************************************
//* DISTINCI�N DE PRIMA                                               *
//*********************************************************************
//PASO590  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRUN,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRSO,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(7,11,CH,A)
/*
//*********************************************************************
//* DISTINCI�N DE PRIMA                                               *
//*********************************************************************
//PASO600  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SJI03,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.CONPRI,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=480,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(07,11,CH,A)
  INCLUDE COND=(18,15,CH,NE,C'               ')
  SUM FIELDS=NONE
/*
//**********************************************************************
//PASO605  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CTPB.EXT.SORT.UNION,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.CONPRI,
//            DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.CTPB.EXT.SORT.UNION.GP,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=178,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,11,A)
 JOINKEYS FILE=F2,FIELDS=(7,11,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,178,F2:1,480,?)
 SORT FIELDS=COPY
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(659,1,CH,EQ,C'B'),
 BUILD=(1,164,324,3,168,11)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//*********************************************************************
//* SUMATORIO DE LOS ASEGURADOS Y LAS PRIMAS TOTALES POR:             *
//* - POLIZA                                                          *
//* - SUBPOLIZA                                                       *
//*********************************************************************
//PASO610  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CTPB.EXT.SORT.UNION.GP,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CTPB.EXT.TASE,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=178,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(01,11,CH,A,165,3,CH,A)
  SUM FIELDS=(149,7,PD)
/*
//**********************************************************************
//PASO620  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINPRI,
//            DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORT,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=232,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTOUT2 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.F1,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=178,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,11,A)
 JOINKEYS FILE=F2,FIELDS=(7,11,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,178,F2:1,550,?)
 SORT FIELDS=COPY
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(729,1,CH,EQ,C'B'),
 BUILD=(1,178,413,54)
 OUTFIL FNAMES=SORTOUT2,INCLUDE=(729,1,CH,EQ,C'1'),
 BUILD=(1,178)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//**********************************************************************
//PASO630  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRSO,
//            DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTNR,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=232,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTOUT2 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.F1NR,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=178,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,11,A)
 JOINKEYS FILE=F2,FIELDS=(7,11,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,178,F2:1,550,?)
 SORT FIELDS=COPY
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(729,1,CH,EQ,C'B'),
 BUILD=(1,58,294,8,67,112,413,54)
 OUTFIL FNAMES=SORTOUT2,INCLUDE=(729,1,CH,EQ,C'1'),
 BUILD=(1,178)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//**********************************************************************
//PASO640  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.SINNRA5,
//            DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTNR1,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=232,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTOUT2 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.F1NR1,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=178,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,11,A),SORTED
 JOINKEYS FILE=F2,FIELDS=(7,11,A),SORTED
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,178,F2:1,550,?)
 SORT FIELDS=COPY
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(729,1,CH,EQ,C'B'),
 BUILD=(1,58,294,8,67,112,413,54)
 OUTFIL FNAMES=SORTOUT2,INCLUDE=(729,1,CH,EQ,C'1'),
 BUILD=(1,178)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//**********************************************************************
//PASO650  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORT,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.CONPRI,
//            DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRI,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=245,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTOUT2 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.F2,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=232,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,11,A)
 JOINKEYS FILE=F2,FIELDS=(7,11,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,232,F2:1,480,?)
 SORT FIELDS=COPY
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(713,1,CH,EQ,C'B'),
 BUILD=(1,232,418,13)
 OUTFIL FNAMES=SORTOUT2,INCLUDE=(713,1,CH,EQ,C'1'),
 BUILD=(1,232)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//**********************************************************************
//PASO670  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTNR,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.CONPRI,
//            DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRIN,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=245,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTOUT2 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.F2NR,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=232,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,11,A)
 JOINKEYS FILE=F2,FIELDS=(7,11,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,232,F2:1,480,?)
 SORT FIELDS=COPY
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(713,1,CH,EQ,C'B'),
 BUILD=(1,232,418,13)
 OUTFIL FNAMES=SORTOUT2,INCLUDE=(713,1,CH,EQ,C'1'),
 BUILD=(1,232)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//**********************************************************************
//PASO680  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTNR1,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.CONPRI,
//            DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRI1,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=245,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTOUT2 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.F2NR1,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=232,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,11,A)
 JOINKEYS FILE=F2,FIELDS=(7,11,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,232,F2:1,480,?)
 SORT FIELDS=COPY
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(713,1,CH,EQ,C'B'),
 BUILD=(1,232,418,13)
 OUTFIL FNAMES=SORTOUT2,INCLUDE=(713,1,CH,EQ,C'1'),
 BUILD=(1,232)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//*********************************************************************
//PASO690  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRI,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRI.S0,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=245,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
  INCLUDE COND=(165,3,CH,NE,C'   ')
/*
//*********************************************************************
//PASO700  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRIN,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRI.S0NR,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=245,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
  INCLUDE COND=(165,3,CH,NE,C'   ')
/*
//*********************************************************************
//PASO710  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRI1,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRI.S0NR1,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=245,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
  INCLUDE COND=(165,3,CH,NE,C'   ')
/*
//**********************************************************************
//PASO720  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRI.S0NR,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323573.B350J028.FICHA,DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRI.FCNR,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=245,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTOUT2 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRI.FCN2,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=245,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(59,8,A)
 JOINKEYS FILE=F2,FIELDS=(43,8,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,245,F2:1,80,?)
 SORT FIELDS=COPY
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(326,1,CH,EQ,C'B'),
 BUILD=(1,245)
 OUTFIL FNAMES=SORTOUT2,INCLUDE=(326,1,CH,EQ,C'1'),
 BUILD=(1,245)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//*********************************************************************
//* SUMATORIO DE LOS ASEGURADOS Y LAS PRIMAS TOTALES POR:             *
//* - POLIZA                                                          *
//* - SUBPOLIZA                                                       *
//*********************************************************************
//PASO730  EXEC PGM=SORT,REGION=0M
//SORTIN   DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRI.S0,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTOUT,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=245,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(165,3,CH,A)
  SUM FIELDS=(171,7,ZD,179,18,ZD,197,18,ZD,215,18,ZD,233,13,ZD)
/*
//*********************************************************************
//* SUMATORIO DE LOS ASEGURADOS Y LAS PRIMAS TOTALES POR:             *
//* - POLIZA                                                          *
//* - SUBPOLIZA                                                       *
//*********************************************************************
//PASO740  EXEC PGM=SORT,REGION=0M
//SORTIN   DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRI.FCNR,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTOUT.NR,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=245,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(165,3,CH,A)
  SUM FIELDS=(171,7,ZD,179,18,ZD,197,18,ZD,215,18,ZD,233,13,ZD)
/*
//**********************************************************************
//PASO750  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRI.FCN2,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRI.S0NR1,
//            DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRI.S0NR2,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=245,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,11,A)
 JOINKEYS FILE=F2,FIELDS=(1,11,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,245,F2:1,245,?)
 SORT FIELDS=COPY
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(491,1,CH,EQ,C'2'),
 BUILD=(246,245)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//*********************************************************************
//PASO760  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRI.FCN2,DISP=SHR
//         DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTPRI.S0NR2,
//            DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTOUT.NR2,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=245,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(165,3,CH,A)
  SUM FIELDS=(171,7,ZD,179,18,ZD,197,18,ZD,215,18,ZD,233,13,ZD)
/*
//**********************************************************************
//PASO770  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTOUT,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTOUT.NR2,
//            DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTOUT.TOT,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=312,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTOUT2 DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTOUT.TOTF1,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=312,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(165,3,A)
 JOINKEYS FILE=F2,FIELDS=(165,3,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,245,F2:1,245,?)
 SORT FIELDS=COPY
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(491,1,CH,EQ,C'B'),
 BUILD=(1,245,424,67)
 OUTFIL FNAMES=SORTOUT2,INCLUDE=(491,1,CH,EQ,C'1'),
 BUILD=(1,245,67X)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//*********************************************************************
//* TRATAMIENTO FICHERO DE TOTALES                                    *
//*********************************************************************
//PASO780  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTOUT.TOT,DISP=SHR
//         DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTOUT.TOTF1,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTOUT.T,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=312,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(165,3,CH,A)
/*
//**********************************************************************
//PASO790  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323567.S2PASO20.SORTOUT,
//***      FICOL LREC=1264
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323575.CTPB.EXT.SORT2,
//            DISP=SHR
//SORTOUT  DD DSN=EXPLO.P0323575.PASO790.JOIN,
//***      FICOL LREC=1264
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=1264,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(2,11,A)
 JOINKEYS FILE=F2,FIELDS=(1,11,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,1264,F2:1,178,?)
 SORT FIELDS=COPY
 SUM FIELDS=NONE
 OUTFIL FNAMES=SORTOUT,INCLUDE=(1443,1,CH,EQ,C'B'),
 BUILD=(1,1264)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*********************************************************************
//PASO800  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323574.B350J084.SYS003,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.B350J084.SYS003.PRU,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//*           DCB=(RECFM=FB,LRECL=120,BLKSIZE=0)
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(5,14,CH,A,31,1,CH,A,76,8,CH,A)
/*
//**********************************************************************
//* ORDENA M�OVIMIENTOS                                               *
//*********************************************************************
//PASO810  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323574.B350JX84.SYS003.BONO,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.B350JX84.SYS003.PRU,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//*           DCB=(RECFM=FB,LRECL=120,BLKSIZE=0)
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(5,14,CH,A,31,1,CH,A,76,8,CH,A)
/*
//**********************************************************************
//* ORDENA M�OVIMIENTOS                                               *
//*********************************************************************
//PASO820  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323574.B350JZ84.SYS003,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.B350JZ84.SYS003.PRU,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//*           DCB=(RECFM=FB,LRECL=120,BLKSIZE=0)
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(5,14,CH,A,31,1,CH,A,76,8,CH,A)
/*
//*********************************************************************
//PASO830  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONOA.GP,DISP=SHR
//         DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURA.GP,DISP=SHR
//         DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURA.TPOLSP,DISP=SHR
//         DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONOA.TOTPOL,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTA.ANT,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=550,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(146,3,CH,A)
/*
//**********************************************************************
//PASO835  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CTPB.EXT.SORT3,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.CONPRI,
//            DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.CTPB.EXT.SORT4,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=194,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,11,A)
 JOINKEYS FILE=F2,FIELDS=(7,11,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,194,F2:1,480,?)
 SORT FIELDS=COPY
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(675,1,CH,EQ,C'B'),
 BUILD=(1,164,340,3,168,27)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//**********************************************************************
//PASO840  EXEC PGM=IKJEFT01,DYNAMNBR=100,REGION=0M
//**********************************************************************
// INCLUDE MEMBER=IDMSDD02
// INCLUDE MEMBER=RCOLVIDA A=N
// INCLUDE MEMBER=RGSCAR   A=N
// INCLUDE MEMBER=RCOSETE  A=N
//*INCLUDE MEMBER=RREDTER  A=N
// INCLUDE MEMBER=RDAGENER A=N
//SYS001   DD DSN=EXPLO.P0323575.CTPB.EXT.SORT4,DISP=SHR
//SYS002   DD DSN=EXPLO.P0323575.B350J084.SYS003.PRU,DISP=SHR
//SYS003   DD DSN=EXPLO.P0323575.CTPB.EXT.TASE.SORTOUT.T,DISP=SHR
//SYS006   DD DSN=EXPLO.P0323573.B350J028.FICHA,DISP=SHR
//SYS007   DD DSN=EXPLO.P0323575.CTPB.EXT.TASE,DISP=SHR
//SYS008   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.CONPRI,DISP=SHR
//SYS011   DD DSN=EXPLO.P0323575.PASO790.JOIN,DISP=SHR
//***      FICOL LREC=1264
//SYS012   DD DSN=EXPLO.P0323575.B350JX84.SYS003.PRU,DISP=SHR
//SYS013   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTB.BONOPR,DISP=SHR
//SYS014   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.PLURI.GP,DISP=SHR
//SYS015   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.BONOR.GP,DISP=SHR
//SYS019   DD DSN=EXPLO.P0323575.B350JZ84.SYS003.PRU,DISP=SHR
//SYS020   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.LTA.ANT,DISP=SHR
//*
//SYS004   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT,
//            DISP=(,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=1400,BLKSIZE=0)
//*
//SYS005   DD DSN=EXPLO.P0323575.CASOUS03.FICHINC,
//            DISP=(,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=800,BLKSIZE=0)
//*
//SYS010   DD DSN=EXPLO.P0323575.CASOUS03.FICHTOT,
//            DISP=(,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=236,BLKSIZE=0)
//*
//*YSOUT   DD SYSOUT=*
//SYSOUT   DD DSN=EXPLO.P0323575.CASOUS03.SYSOUT.LT1,
//            DISP=(,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(5000,1500),RLSE),
//            DCB=(RECFM=FB,LRECL=133,BLKSIZE=0)
//*
//SYSDBOUT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SYSTSIN  DD *
 DSN SYSTEM(DSNR)
 RUN PROGRAM(B350JI21) PLAN(PLURBAT)
END
/*
//*
//*********************************************************************
//PASO850  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTJNF1 DD DSN=EXPLO.P0323575.CASOUS03.FICHTOT,
//            DISP=SHR
//SORTJNF2 DD DSN=EXPLO.P0323573.B350J028.FICHA,
//            DISP=SHR
//SORTOUT1 DD DSN=EXPLO.P0323575.CASOUS03.FICHTOT.AMB,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=236,BLKSIZE=0),UNIT=(SYSDA,3)
//SORTOUT2 DD DSN=EXPLO.P0323575.CASOUS03.FICHTOT.F1,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=236,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(54,8,A)
 JOINKEYS FILE=F2,FIELDS=(43,8,A)
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,236,F2:1,80,?)
 SORT FIELDS=COPY
 OUTFIL FNAMES=SORTOUT1,INCLUDE=(317,1,CH,EQ,C'B'),
 BUILD=(1,236)
 OUTFIL FNAMES=SORTOUT2,INCLUDE=(317,1,CH,EQ,C'1'),
 BUILD=(1,57,C'9999',62,175)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//*********************************************************************
//* TRATAMIENTO FICHERO DE TOTALES                                    *
//*********************************************************************
//PASO860  EXEC PGM=SORT,REGION=0M
//*********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHTOT.AMB,DISP=SHR
//         DD DSN=EXPLO.P0323575.CASOUS03.FICHTOT.F1,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHOT.SORT,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=236,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(1,2,CH,A,3,3,CH,A,6,3,CH,A,9,1,CH,A,10,4,CH,A,14,39,CH,A,
              54,8,CH,A)
  SUM FIELDS=(62,13,ZD,75,18,ZD,93,18,ZD,111,18,ZD,129,18,ZD,147,18,ZD,
              165,18,ZD,183,18,ZD,201,18,ZD,219,18,ZD)
/*
//**********************************************************************
//*//PASO870  EXEC PGM=IKJEFT01,DYNAMNBR=100,REGION=0M
//**********************************************************************
//*//SYSTSIN  DD *
//* DSN SYSTEM(DSNR)
//* RUN PROGRAM(B350J090) PLAN(PLCSBAT)
//*END
//*//SYSTSPRT DD SYSOUT=*
//*//SYSPRINT DD SYSOUT=*
//*//SYSUDUMP DD SYSOUT=*
//*//SYSOUT   DD SYSOUT=*
//*//SYSDBOUT DD SYSOUT=*
//*//SYPERINT DD SYSOUT=*
//*//ABENDAID DD SYSOUT=*
//*//SYSABEND DD SYSOUT=*
//*//SYS001   DD DSN=EXPLO.P0323575.CASOUS03.FICHOT.SORT,
//*//            DISP=SHR
//*//SYS003   DD DSN=EXPLO.P0323573.B350J028.FICHA,
//*//            DISP=SHR
//*//SYS002   DD DSN=EXPLO.P0323575.CASOUS03.FICHTOT.SORT1,
//*//            DISP=(NEW,CATLG,DELETE),
//*//            UNIT=SYSDA,SPACE=(TRK,(1500,500),RLSE),
//*//            DCB=(DSORG=PS,RECFM=FB,LRECL=650,BLKSIZE=0)
//*/*
//**********************************************************************
//PASO870  EXEC PGM=B350J090,REGION=0M
//**********************************************************************
//SYS001   DD DSN=EXPLO.P0323575.CASOUS03.FICHOT.SORT,
//            DISP=SHR
//SYS003   DD DSN=EXPLO.P0323573.B350J028.FICHA,
//            DISP=SHR
//SYS002   DD DSN=EXPLO.P0323575.CASOUS03.FICHTOT.SORT1,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(TRK,(1500,500),RLSE),
//            DCB=(DSORG=PS,RECFM=FB,LRECL=650,BLKSIZE=0)
//SYSDBOUT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//*
//**********************************************************************
//PASO880  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//SORTIN   DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT,DISP=SHR
//*
//SORTOUT  DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT.ENVFTP,
//            DISP=(NEW,CATLG,DELETE),UNIT=(SYSDA,3),
//            SPACE=(TRK,(500,500),RLSE),
//*           DCB=(RECFM=FB,LRECL=800,BLKSIZE=0)
//            DCB=*.SORTIN
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
/*
//**********************************************************************
//PASO890  EXEC PGM=B350J105,REGION=0M
//**********************************************************************
//SYSIN    DD *
%%F1
//SYS001   DD DSN=EXPLO.P0323573.B350J028.FICHA,DISP=SHR
//*YS002   DD DSN=EXPLO.P0323574.B350J084.SYS003,DISP=SHR
//SYS002   DD DSN=EXPLO.P0323574.B350J108.SYS003,DISP=SHR
//*YS003   DD DSN=EXPLO.P0323575.B350J085.SYS003.PRUEBA,
//SYS003   DD DSN=EXPLO.P0323575.B350J105.SYS003.PRUEBA,
//         DISP=(NEW,CATLG,DELETE),
//         UNIT=SYSDA,SPACE=(TRK,(1500,500),RLSE),
//         DCB=(DSORG=PS,RECFM=FB,LRECL=650,BLKSIZE=0)
//SYSDBOUT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//*
//**********************************************************************
//PASO900  EXEC PGM=B350J105,REGION=0M
//**********************************************************************
//SYSIN    DD *
%%F1
//SYS001   DD DSN=EXPLO.P0323573.B350J028.FICHA,DISP=SHR
//*YS002   DD DSN=EXPLO.P0323574.B350JX84.SYS003.BONO,DISP=SHR
//SYS002   DD DSN=EXPLO.P0323574.B350JX08.SYS003.BONO,DISP=SHR
//*YS003   DD DSN=EXPLO.P0323575.B350JX85.SYS003.PRUEBA,
//SYS003   DD DSN=EXPLO.P0323575.B350JX05.SYS003.PRUEBA,
//         DISP=(NEW,CATLG,DELETE),
//         UNIT=SYSDA,SPACE=(TRK,(1500,500),RLSE),
//         DCB=(DSORG=PS,RECFM=FB,LRECL=650,BLKSIZE=0)
//SYSDBOUT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//**********************************************************************
//PASO910  EXEC PGM=B350J105,REGION=0M
//**********************************************************************
//SYSIN    DD *
%%F1
//SYS001   DD DSN=EXPLO.P0323573.B350J028.FICHA,DISP=SHR
//*YS002   DD DSN=EXPLO.P0323574.B350JZ84.SYS003,DISP=SHR
//SYS002   DD DSN=EXPLO.P0323574.B350JZ08.SYS003,DISP=SHR
//*YS003   DD DSN=EXPLO.P0323575.B350JZ85.SYS003.PRUEBA,
//SYS003   DD DSN=EXPLO.P0323575.B350JZ05.SYS003.PRUEBA,
//         DISP=(NEW,CATLG,DELETE),
//         UNIT=SYSDA,SPACE=(TRK,(1500,500),RLSE),
//         DCB=(DSORG=PS,RECFM=FB,LRECL=650,BLKSIZE=0)
//SYSDBOUT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//**********************************************************************
//PASO920  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* DESDOBLAMOS SINIESTROS DEL RESTO DEL LISTADO
//**********************************************************************
//*
//SORTIN   DD  DSN=EXPLO.P0323574.PASO290.SORT.AMBOS,DISP=SHR
//*INCLUYE AL FINAL ETIQUETA SIDMS/SNEO PARA USO POSTERIOR
//SORTOUT  DD  DSN=EXPLO.P0323575.PASO920.SORT,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(500,100),RLSE),
//             DCB=(RECFM=FB,LRECL=255,BLKSIZE=0)
//*
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(1,10,A,11,4,A,15,9,A,150,8,A,117,1,D),FORMAT=BI
 INCLUDE COND=(117,1,CH,NE,C' ')
 OUTFIL IFTHEN=(WHEN=(117,1,CH,EQ,C'I'),BUILD=(1,247,C' SIDMS  ')),
        IFTHEN=(WHEN=(117,1,CH,EQ,C'2'),BUILD=(1,247,C' SIDMS2 ')),
        IFTHEN=(WHEN=(117,1,CH,EQ,C'3'),BUILD=(1,247,C' SIDMS3 ')),
        IFTHEN=(WHEN=(117,1,CH,EQ,C'N'),BUILD=(1,247,C' SNEO   ')),
        IFTHEN=(WHEN=(117,1,CH,EQ,C' '),BUILD=(1,247,C'        '))
/*
//*
//**********************************************************************
//PASO930   EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* RENUMERO REGISTROS DEL LISTADO PARA "DESDOBLARLO"
//**********************************************************************
//SORTIN   DD  DSN=EXPLO.P0323575.B350J105.SYS003.PRUEBA,DISP=SHR
//SORTOUT  DD  DSN=EXPLO.P0323575.PASO930.B350J105.LISTNUM,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(1500,500),RLSE),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=690,BLKSIZE=0)
//*            DCB=*.SORTIN
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
 OUTREC BUILD=(SEQNUM,6,ZD,C' 000 ',104,27,C'  ',1,650)
/*
//*********************************************************************
//PASO940  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* DESDOBLAMOS SINIESTROS DEL RESTO DEL LISTADO
//**********************************************************************
//*
//SORTIN   DD  DSN=EXPLO.P0323575.PASO930.B350J105.LISTNUM,DISP=SHR
//* EL FICHERO ESTA ORDENADO (46,10,57,4,18,9) - POL, SUP, EXPEDTE
//SORTSIN  DD  DSN=EXPLO.P0323575.PASO940.B350J105.LIST0206,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(500,100),RLSE),
//             DCB=(RECFM=FB,LRECL=690,BLKSIZE=0)
//*
//SORTRES  DD  DSN=EXPLO.P0323575.PASO940.B350J105.LISTREST,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(500,100),RLSE),
//             DCB=(RECFM=FB,LRECL=690,BLKSIZE=0)
//*
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
 OPTION COPY
  OUTFIL FNAMES=SORTSIN,INCLUDE=(13,4,CH,EQ,C'0206')
  OUTFIL FNAMES=SORTRES,INCLUDE=(13,4,CH,NE,C'0206')
 END
/*
//*
//*********************************************************************
//PASO950  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* DESDOBLAMOS SINIESTROS DEL RESTO DEL LISTADO
//**********************************************************************
//*
//SORTIN   DD  DSN=EXPLO.P0323575.PASO940.B350J105.LIST0206,DISP=SHR
//* EL FICHERO ESTA ORDENADO (46,10,57,4,18,9) - POL, SUP, EXPEDTE
//SORTOUT  DD  DSN=EXPLO.P0323575.PASO950.B350J105.LIST0206,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(500,100),RLSE),
//             DCB=(RECFM=FB,LRECL=690,BLKSIZE=0)
//*
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(46,10,A,57,4,A,18,9,A),FORMAT=BI
/*
//*
//**********************************************************************
//PASO960   EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* CRUCE SINIESTROS EXTENDIDO CON LINEAS-LISTADO SINIESTRO
//*******************************************************************
//*COPY R350TE26 + EXTENSION ETIQUETA 8 POS
//SORTJNF1 DD DSN=EXPLO.P0323575.PASO920.SORT,
//            DISP=SHR
//*LISTADO MOVIMIENTOS: SOLO REGISTRO 0206
//SORTJNF2 DD DSN=EXPLO.P0323575.PASO950.B350J105.LIST0206,
//            DISP=SHR
//AMBOS1   DD DSN=EXPLO.P0323575.PASO960.JOIN.AMBOS,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=690,BLKSIZE=0),UNIT=(SYSDA,3)
//AMBOS2   DD DSN=EXPLO.P0323575.PASO960.JOIN.AMBOSDEC,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=730,BLKSIZE=0),UNIT=(SYSDA,3)
//F1       DD DSN=EXPLO.P0323575.PASO960.JOIN.F1,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=255,BLKSIZE=0),UNIT=(SYSDA,3)
//F2       DD DSN=EXPLO.P0323575.PASO960.JOIN.F2,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=690,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,10,A,11,4,A,15,9,A),SORTED
 JOINKEYS FILE=F2,FIELDS=(46,10,A,57,4,A,18,9,A),SORTED
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,255,F2:1,690,?)
 SORT FIELDS=COPY

 OUTFIL FNAMES=AMBOS1,INCLUDE=(946,1,CH,EQ,C'B'),
 BUILD=(256,690)
 OUTFIL FNAMES=AMBOS2,INCLUDE=(946,1,CH,EQ,C'B'),
 BUILD=(1,23,150,8,249,6,C'   ',256,197,
        C'00000000;                  ;                  ;',
        500,2,C';',249,6,C';',159,8,C';',150,8,C';',
        168,13,ZD,EDIT=(SI.III.IIT,TT),SIGNS=(,-),C';',
        182,13,ZD,EDIT=(SI.III.IIT,TT),SIGNS=(,-),C';',
        119,4,C';',385C' ')
 OUTFIL FNAMES=F1,INCLUDE=(946,1,CH,EQ,C'1'),
 BUILD=(1,255)
 OUTFIL FNAMES=F2,INCLUDE=(946,1,CH,EQ,C'2'),
 BUILD=(256,690)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*
//*********************************************************************
//PASO970  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* DESDOBLAMOS SINIESTROS DEL RESTO DEL LISTADO
//**********************************************************************
//*
//SORTIN   DD  DSN=EXPLO.P0323575.PASO960.JOIN.AMBOSDEC,DISP=SHR
//* EL FICHERO ESTA ORDENADO (46,10,57,4,18,9) - POL, SUP, EXPEDTE
//SORTOUT  DD  DSN=EXPLO.P0323575.PASO970.JOIN.AMBOSDEC,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(500,100),RLSE),
//             DCB=(RECFM=FB,LRECL=690,BLKSIZE=0)
//*
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(1,40,A),FORMAT=BI
 INREC IFTHEN=(WHEN=INIT,
                OVERLAY=(48:SEQNUM,3,ZD,START=1,RESTART=(1,23)))
 OUTREC FIELDS=(41,690)
/*
//*
//*********************************************************************
//PASO980  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* DESDOBLAMOS SINIESTROS DEL RESTO DEL LISTADO
//**********************************************************************
//*
//SORTIN   DD  DSN=EXPLO.P0323575.PASO950.B350J105.LIST0206,DISP=SHR
//         DD  DSN=EXPLO.P0323575.PASO940.B350J105.LISTREST,DISP=SHR
//         DD  DSN=EXPLO.P0323575.PASO970.JOIN.AMBOSDEC,DISP=SHR
//* EL FICHERO ESTA ORDENADO (46,10,57,4,18,9) - POL, SUP, EXPEDTE
//SORTOUT  DD  DSN=EXPLO.P0323575.B350J085.SYS003.PRUEBA,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(500,100),RLSE),
//             DCB=(RECFM=FB,LRECL=650,BLKSIZE=0)
//*
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(1,6,A,8,3,A),FORMAT=BI
 OUTREC FIELDS=(41,650)
/*
//**********************************************************************
//PASO930X  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* RENUMERO REGISTROS DEL LISTADO PARA "DESDOBLARLO"
//**********************************************************************
//SORTIN   DD  DSN=EXPLO.P0323575.B350JX05.SYS003.PRUEBA,DISP=SHR
//SORTOUT  DD  DSN=EXPLO.P0323575.PASO930X.B350JX05.LISTNUM,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(1500,500),RLSE),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=690,BLKSIZE=0)
//*            DCB=*.SORTIN
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
 OUTREC BUILD=(SEQNUM,6,ZD,C' 000 ',104,27,C'  ',1,650)
/*
//*********************************************************************
//PASO940X EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* DESDOBLAMOS SINIESTROS DEL RESTO DEL LISTADO
//**********************************************************************
//*
//SORTIN   DD  DSN=EXPLO.P0323575.PASO930X.B350JX05.LISTNUM,DISP=SHR
//* EL FICHERO ESTA ORDENADO (46,10,57,4,18,9) - POL, SUP, EXPEDTE
//SORTSIN  DD  DSN=EXPLO.P0323575.PASO940X.B350JX05.LIST0206,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(500,100),RLSE),
//             DCB=(RECFM=FB,LRECL=690,BLKSIZE=0)
//*
//SORTRES  DD  DSN=EXPLO.P0323575.PASO940X.B350JX05.LISTREST,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(500,100),RLSE),
//             DCB=(RECFM=FB,LRECL=690,BLKSIZE=0)
//*
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
 OPTION COPY
  OUTFIL FNAMES=SORTSIN,INCLUDE=(13,4,CH,EQ,C'0206')
  OUTFIL FNAMES=SORTRES,INCLUDE=(13,4,CH,NE,C'0206')
 END
/*
//*
//*********************************************************************
//PASO950X EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* DESDOBLAMOS SINIESTROS DEL RESTO DEL LISTADO
//**********************************************************************
//*
//SORTIN   DD  DSN=EXPLO.P0323575.PASO940X.B350JX05.LIST0206,DISP=SHR
//* EL FICHERO ESTA ORDENADO (46,10,57,4,18,9) - POL, SUP, EXPEDTE
//SORTOUT  DD  DSN=EXPLO.P0323575.PASO950X.B350JX05.LIST0206,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(500,100),RLSE),
//             DCB=(RECFM=FB,LRECL=690,BLKSIZE=0)
//*
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(46,10,A,57,4,A,18,9,A),FORMAT=BI
/*
//*
//**********************************************************************
//PASO960X  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* CRUCE SINIESTROS EXTENDIDO CON LINEAS-LISTADO SINIESTRO
//*******************************************************************
//*COPY R350TE26 + EXTENSION ETIQUETA 8 POS
//SORTJNF1 DD DSN=EXPLO.P0323575.PASO920.SORT,
//            DISP=SHR
//*LISTADO MOVIMIENTOS: SOLO REGISTRO 0206
//SORTJNF2 DD DSN=EXPLO.P0323575.PASO950X.B350JX05.LIST0206,
//            DISP=SHR
//AMBOS1   DD DSN=EXPLO.P0323575.PASO960X.JOIN.AMBOS,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=690,BLKSIZE=0),UNIT=(SYSDA,3)
//AMBOS2   DD DSN=EXPLO.P0323575.PASO960X.JOIN.AMBOSDEC,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=730,BLKSIZE=0),UNIT=(SYSDA,3)
//F1       DD DSN=EXPLO.P0323575.PASO960X.JOIN.F1,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=255,BLKSIZE=0),UNIT=(SYSDA,3)
//F2       DD DSN=EXPLO.P0323575.PASO960X.JOIN.F2,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=690,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,10,A,11,4,A,15,9,A),SORTED
 JOINKEYS FILE=F2,FIELDS=(46,10,A,57,4,A,18,9,A),SORTED
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,255,F2:1,690,?)
 SORT FIELDS=COPY

 OUTFIL FNAMES=AMBOS1,INCLUDE=(946,1,CH,EQ,C'B'),
 BUILD=(256,690)
 OUTFIL FNAMES=AMBOS2,INCLUDE=(946,1,CH,EQ,C'B'),
 BUILD=(1,23,150,8,249,6,C'   ',256,197,
        C'00000000;                  ;                  ;',
        500,2,C';',249,6,C';',159,8,C';',150,8,C';',
        168,13,ZD,EDIT=(SI.III.IIT,TT),SIGNS=(,-),C';',
        182,13,ZD,EDIT=(SI.III.IIT,TT),SIGNS=(,-),C';',
        119,4,C';',385C' ')
 OUTFIL FNAMES=F1,INCLUDE=(946,1,CH,EQ,C'1'),
 BUILD=(1,255)
 OUTFIL FNAMES=F2,INCLUDE=(946,1,CH,EQ,C'2'),
 BUILD=(256,690)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*
//*********************************************************************
//PASO970X EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* DESDOBLAMOS SINIESTROS DEL RESTO DEL LISTADO
//**********************************************************************
//*
//SORTIN   DD  DSN=EXPLO.P0323575.PASO960X.JOIN.AMBOSDEC,DISP=SHR
//* EL FICHERO ESTA ORDENADO (46,10,57,4,18,9) - POL, SUP, EXPEDTE
//SORTOUT  DD  DSN=EXPLO.P0323575.PASO970X.JOIN.AMBOSDEC,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(500,100),RLSE),
//             DCB=(RECFM=FB,LRECL=690,BLKSIZE=0)
//*
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(1,40,A),FORMAT=BI
 INREC IFTHEN=(WHEN=INIT,
                OVERLAY=(48:SEQNUM,3,ZD,START=1,RESTART=(1,23)))
 OUTREC FIELDS=(41,690)
/*
//*
//*********************************************************************
//PASO980X EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* DESDOBLAMOS SINIESTROS DEL RESTO DEL LISTADO
//**********************************************************************
//*
//SORTIN   DD  DSN=EXPLO.P0323575.PASO950X.B350JX05.LIST0206,DISP=SHR
//         DD  DSN=EXPLO.P0323575.PASO940X.B350JX05.LISTREST,DISP=SHR
//         DD  DSN=EXPLO.P0323575.PASO970X.JOIN.AMBOSDEC,DISP=SHR
//* EL FICHERO ESTA ORDENADO (46,10,57,4,18,9) - POL, SUP, EXPEDTE
//SORTOUT  DD  DSN=EXPLO.P0323575.B350JX85.SYS003.PRUEBA,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(500,100),RLSE),
//             DCB=(RECFM=FB,LRECL=650,BLKSIZE=0)
//*
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(1,6,A,8,3,A),FORMAT=BI
 OUTREC FIELDS=(41,650)
/*
//**********************************************************************
//PASO930Z  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* RENUMERO REGISTROS DEL LISTADO PARA "DESDOBLARLO"
//**********************************************************************
//SORTIN   DD  DSN=EXPLO.P0323575.B350JZ05.SYS003.PRUEBA,DISP=SHR
//SORTOUT  DD  DSN=EXPLO.P0323575.PASO930Z.B350JZ05.LISTNUM,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(1500,500),RLSE),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=690,BLKSIZE=0)
//*            DCB=*.SORTIN
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
 OUTREC BUILD=(SEQNUM,6,ZD,C' 000 ',104,27,C'  ',1,650)
/*
//*********************************************************************
//PASO940Z EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* DESDOBLAMOS SINIESTROS DEL RESTO DEL LISTADO
//**********************************************************************
//*
//SORTIN   DD  DSN=EXPLO.P0323575.PASO930Z.B350JZ05.LISTNUM,DISP=SHR
//* EL FICHERO ESTA ORDENADO (46,10,57,4,18,9) - POL, SUP, EXPEDTE
//SORTSIN  DD  DSN=EXPLO.P0323575.PASO940Z.B350JZ05.LIST0206,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(500,100),RLSE),
//             DCB=(RECFM=FB,LRECL=690,BLKSIZE=0)
//*
//SORTRES  DD  DSN=EXPLO.P0323575.PASO940Z.B350JZ05.LISTREST,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(500,100),RLSE),
//             DCB=(RECFM=FB,LRECL=690,BLKSIZE=0)
//*
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
 OPTION COPY
  OUTFIL FNAMES=SORTSIN,INCLUDE=(13,4,CH,EQ,C'0206')
  OUTFIL FNAMES=SORTRES,INCLUDE=(13,4,CH,NE,C'0206')
 END
/*
//*
//*********************************************************************
//PASO950Z EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* DESDOBLAMOS SINIESTROS DEL RESTO DEL LISTADO
//**********************************************************************
//*
//SORTIN   DD  DSN=EXPLO.P0323575.PASO940Z.B350JZ05.LIST0206,DISP=SHR
//* EL FICHERO ESTA ORDENADO (46,10,57,4,18,9) - POL, SUP, EXPEDTE
//SORTOUT  DD  DSN=EXPLO.P0323575.PASO950Z.B350JZ05.LIST0206,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(500,100),RLSE),
//             DCB=(RECFM=FB,LRECL=690,BLKSIZE=0)
//*
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(46,10,A,57,4,A,18,9,A),FORMAT=BI
/*
//*
//**********************************************************************
//PASO960Z  EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* CRUCE SINIESTROS EXTENDIDO CON LINEAS-LISTADO SINIESTRO
//*******************************************************************
//*COPY R350TE26 + EXTENSION ETIQUETA 8 POS
//SORTJNF1 DD DSN=EXPLO.P0323575.PASO920.SORT,
//            DISP=SHR
//*LISTADO MOVIMIENTOS: SOLO REGISTRO 0206
//SORTJNF2 DD DSN=EXPLO.P0323575.PASO950Z.B350JZ05.LIST0206,
//            DISP=SHR
//AMBOS1   DD DSN=EXPLO.P0323575.PASO960Z.JOIN.AMBOS,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=690,BLKSIZE=0),UNIT=(SYSDA,3)
//AMBOS2   DD DSN=EXPLO.P0323575.PASO960Z.JOIN.AMBOSDEC,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=730,BLKSIZE=0),UNIT=(SYSDA,3)
//F1       DD DSN=EXPLO.P0323575.PASO960Z.JOIN.F1,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=255,BLKSIZE=0),UNIT=(SYSDA,3)
//F2       DD DSN=EXPLO.P0323575.PASO960Z.JOIN.F2,
//            DISP=(,CATLG,DELETE),SPACE=(TRK,(1000,500),RLSE),
//            DCB=(RECFM=FB,LRECL=690,BLKSIZE=0),UNIT=(SYSDA,3)
//SYSIN    DD *
 JOINKEYS FILE=F1,FIELDS=(1,10,A,11,4,A,15,9,A),SORTED
 JOINKEYS FILE=F2,FIELDS=(46,10,A,57,4,A,18,9,A),SORTED
 JOIN UNPAIRED,F1,F2
 REFORMAT FIELDS=(F1:1,255,F2:1,690,?)
 SORT FIELDS=COPY

 OUTFIL FNAMES=AMBOS1,INCLUDE=(946,1,CH,EQ,C'B'),
 BUILD=(256,690)
 OUTFIL FNAMES=AMBOS2,INCLUDE=(946,1,CH,EQ,C'B'),
 BUILD=(1,23,150,8,249,6,C'   ',256,197,
        C'00000000;                  ;                  ;',
        500,2,C';',249,6,C';',159,8,C';',150,8,C';',
        168,13,ZD,EDIT=(SI.III.IIT,TT),SIGNS=(,-),C';',
        182,13,ZD,EDIT=(SI.III.IIT,TT),SIGNS=(,-),C';',
        119,4,C';',385C' ')
 OUTFIL FNAMES=F1,INCLUDE=(946,1,CH,EQ,C'1'),
 BUILD=(1,255)
 OUTFIL FNAMES=F2,INCLUDE=(946,1,CH,EQ,C'2'),
 BUILD=(256,690)
/*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
//*********************************************************************
//PASO970Z EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* DESDOBLAMOS SINIESTROS DEL RESTO DEL LISTADO
//**********************************************************************
//*
//SORTIN   DD  DSN=EXPLO.P0323575.PASO960Z.JOIN.AMBOSDEC,DISP=SHR
//* EL FICHERO ESTA ORDENADO (46,10,57,4,18,9) - POL, SUP, EXPEDTE
//SORTOUT  DD  DSN=EXPLO.P0323575.PASO970Z.JOIN.AMBOSDEC,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(500,100),RLSE),
//             DCB=(RECFM=FB,LRECL=690,BLKSIZE=0)
//*
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(1,40,A),FORMAT=BI
 INREC IFTHEN=(WHEN=INIT,
                OVERLAY=(48:SEQNUM,3,ZD,START=1,RESTART=(1,23)))
 OUTREC FIELDS=(41,690)
/*
//*
//*********************************************************************
//PASO980Z EXEC PGM=SORT,REGION=0M
//**********************************************************************
//* DESDOBLAMOS SINIESTROS DEL RESTO DEL LISTADO
//**********************************************************************
//*
//SORTIN   DD  DSN=EXPLO.P0323575.PASO950Z.B350JZ05.LIST0206,DISP=SHR
//         DD  DSN=EXPLO.P0323575.PASO940Z.B350JZ05.LISTREST,DISP=SHR
//         DD  DSN=EXPLO.P0323575.PASO970Z.JOIN.AMBOSDEC,DISP=SHR
//* EL FICHERO ESTA ORDENADO (46,10,57,4,18,9) - POL, SUP, EXPEDTE
//SORTOUT  DD  DSN=EXPLO.P0323575.B350JZ85.SYS003.PRUEBA,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(500,100),RLSE),
//             DCB=(RECFM=FB,LRECL=650,BLKSIZE=0)
//*
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(1,6,A,8,3,A),FORMAT=BI
 OUTREC FIELDS=(41,650)
/*
//*
//**************************************************
//PASO990  EXEC PGM=SORT,REGION=0M
//**************************************************
//SORTIN   DD  DSN=EXPLO.P0323573.B340J011.FICHA,DISP=SHR
//SORTOUT  DD  DSN=EXPLO.P0323573.B340J011.FICHA,
//             DISP=SHR
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
  SORT FIELDS=COPY
  OUTREC IFTHEN=(WHEN=(1,2,CH,EQ,C'01'),OVERLAY=(1,534,C'SI'))
/*
//**********************************************************************
//* ENVIO FICHEROS A LAS NAS DE PRODUCCION                             *
//**********************************************************************
//PASOFTP1 EXEC PGM=IKJEFT01,DYNAMNBR=20
//**********************************************************************
//** FICHERO DE UOA                                                    *
//**********************************************************************
//*** STEPLIB DD DSN=SYS1.CMDLIB,DISP=SHR ***
//SYSPROC  DD DSN=SYS2.FTP.REXX,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSMDUMP DD  SYSOUT=*
//SYSTSIN  DD  *
 EX 'SYS2.FTP.REXX(FTPSENDP)' 'HOST(SUVDFTP) +
 FINPUT(EXPLO.P0323575.CASOUS03.FICHPBT.ENVFTP) +
 DIR(/TRANS007/MAV/TECNICO/HOSTSERV/) CNVT(YES) +
 FOUT(%%F1%%._FICH_UOA.TXT) TYPE(TEXT)'
//**********************************************************************
//* ENVIO FICHERO FTP                                                  *
//**********************************************************************
//PASOFTP2 EXEC PGM=IKJEFT01,DYNAMNBR=20
//**********************************************************************
//** FICHERO DE TOTALES                                                *
//**********************************************************************
//*** STEPLIB DD DSN=SYS1.CMDLIB,DISP=SHR ***
//SYSPROC  DD DSN=SYS2.FTP.REXX,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSMDUMP DD  SYSOUT=*
//SYSTSIN  DD  *
 EX 'SYS2.FTP.REXX(FTPSENDP)' 'HOST(SUVDFTP) +
 FINPUT(EXPLO.P0323575.CASOUS03.FICHTOT.SORT1) +
 DIR(/TRANS007/MAV/TECNICO/HOSTSERV/) CNVT(YES) +
 FOUT(%%F1%%._FICH_TOTALES.TXT) TYPE(TEXT)'
//**********************************************************************
//PASOFTP3 EXEC PGM=IKJEFT01,DYNAMNBR=20
//**********************************************************************
//** FICHERO DE INCIDENCIAS UOA                                        *
//**********************************************************************
//*** STEPLIB DD DSN=SYS1.CMDLIB,DISP=SHR ***
//SYSPROC  DD DSN=SYS2.FTP.REXX,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSMDUMP DD  SYSOUT=*
//SYSTSIN  DD  *
 EX 'SYS2.FTP.REXX(FTPSENDP)' 'HOST(SUVDFTP) +
 FINPUT(EXPLO.P0323575.CASOUS03.FICHINC) +
 DIR(/TRANS007/MAV/TECNICO/HOSTSERV/) CNVT(YES) +
 FOUT(%%F1%%._FIC_INCIDEN.TXT) TYPE(TEXT)'
//*
//**********************************************************************
//PASOFTP4 EXEC PGM=IKJEFT01,DYNAMNBR=20
//**********************************************************************
//** FICHERO DE MOVIMIENTOS PLURI.NORMAL                               *
//**********************************************************************
//*** STEPLIB DD DSN=SYS1.CMDLIB,DISP=SHR ***
//SYSPROC  DD DSN=SYS2.FTP.REXX,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSMDUMP DD  SYSOUT=*
//SYSTSIN  DD  *
 EX 'SYS2.FTP.REXX(FTPSENDP)' 'HOST(SUVDFTP) +
 FINPUT(EXPLO.P0323575.B350J085.SYS003.PRUEBA) +
 DIR(/TRANS007/MAV/TECNICO/HOSTSERV/) CNVT(YES) +
 FOUT(%%F1%%._MOV_PLURI_NORMAL.TXT) TYPE(TEXT)'
//*
//**********************************************************************
//PASOFTP5 EXEC PGM=IKJEFT01,DYNAMNBR=20
//**********************************************************************
//** FICHERO DE MOVIMIENTOS BONO                                       *
//**********************************************************************
//*** STEPLIB DD DSN=SYS1.CMDLIB,DISP=SHR ***
//SYSPROC  DD DSN=SYS2.FTP.REXX,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSMDUMP DD  SYSOUT=*
//SYSTSIN  DD  *
 EX 'SYS2.FTP.REXX(FTPSENDP)' 'HOST(SUVDFTP) +
 FINPUT(EXPLO.P0323575.B350JX85.SYS003.PRUEBA) +
 DIR(/TRANS007/MAV/TECNICO/HOSTSERV/) CNVT(YES) +
 FOUT(%%F1%%._MOV_BONO.TXT) TYPE(TEXT)'
//**********************************************************************
//PASOFTP6 EXEC PGM=IKJEFT01,DYNAMNBR=20
//**********************************************************************
//** FICHERO DE MOVIMIENTOS ANTERIOR                                  *
//**********************************************************************
//*** STEPLIB DD DSN=SYS1.CMDLIB,DISP=SHR ***
//SYSPROC  DD DSN=SYS2.FTP.REXX,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSMDUMP DD  SYSOUT=*
//SYSTSIN  DD  *
 EX 'SYS2.FTP.REXX(FTPSENDP)' 'HOST(SUVDFTP) +
 FINPUT(EXPLO.P0323575.B350JZ85.SYS003.PRUEBA) +
 DIR(/TRANS007/MAV/TECNICO/HOSTSERV/) CNVT(YES) +
 FOUT(%%F1%%._MOV_ANTERIOR.TXT) TYPE(TEXT)'
//*
//**********************************************************************
//* ENVIO FICHEROS A CONTROL-D                                         *
//**********************************************************************
//*------------------------------------------------------------*
//PASOCT1  EXEC PGM=ICEGENER
//*-------------------------------------------------------------------
//SYSUT1    DD DSN=EXPLO.P0323575.CASOUS03.FICHPBT,
//          DISP=SHR
//CTD      OUTPUT DEST=CTD1
//SYSUT2    DD SYSOUT=(E,,STA1),CHARS=(QCL0,QUL0),FREE=CLOSE,
//          DCB=OPTCD=J,
//          OUTPUT=*.CTD
//SYSPRINT  DD SYSOUT=*
//SYSIN    DD  DUMMY
/*
//**
//*-------------------------------------------------------------------
//PASOCT2  EXEC PGM=ICEGENER
//*-------------------------------------------------------------------
//SYSUT1    DD DSN=EXPLO.P0323575.CASOUS03.FICHTOT.SORT1,
//          DISP=SHR
//CTD      OUTPUT DEST=CTD2
//SYSUT2    DD SYSOUT=(E,,STA1),CHARS=(QCL0,QUL0),FREE=CLOSE,
//          DCB=OPTCD=J,
//          OUTPUT=*.CTD
//SYSPRINT  DD SYSOUT=*
//SYSIN    DD  DUMMY
/*
//*==============================================================*
//*  FORCE DE LA DECOLLATING EN CONTROL-D                        *
//*==============================================================*
//* %%SET %%FE=%%DAY.%%MONTH.%%YEAR
//REPDAILY  EXEC CTDPLAND,PROG=CTDRRQ,
//  PARM='%%FE SYS2.CTD.REPORTS %%JOBNAME * FORCE'
//******************************************************
//PASOXMI1 EXEC PGM=IKJEFT1B
//******************************************************
//SYSEXEC  DD DISP=SHR,DSN=SYS2.XMITIP.EXEC
//*SYSTCPD DD DSN=SYS2.TCPIP.TCPPARMS(TCPDATA),DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//SYSMDUMP DD SYSOUT=*
//DESTINO  DD *
TO DMAVISATARPB@MAPFRE.COM
TO GBARRE@MAPFRE.COM
TO josemaria.ubeda@nfoque.as
TO SUSARTE@MAPFRE.COM
TO MMSAN@MAPFRE.COM
//MENSAJE  DD  *
 FIN OK  GENERACION FICHEROS CADENA X0323575 PB TECNICA
 PARA EL CIERRE %%F1

 YA ESTAN A VUESTRA EN EL SERVIDOR

            TRANS007\MAV\TECNICO\HOSTSERV

   LOS FICHEROS
        %%F1%%._FICH_UOA.TXT
        %%F1%%._FICH_TOTALES.TXT
        %%F1%%._FIC_INCIDEN.TXT
        %%F1%%._MOV_PLURI_NORMAL.TXT
        %%F1%%._MOV_BONO.TXT
        %%F1%%._MOV_ANTERIOR.TXT

 Y EN CONTROL-D
        FUOAPBTD.TXT
        FTOTPBTD.TXT

 UN SALUDO
//SYSTSIN  DD  *
%XMITIP -
   * AddressFileDD DESTINO -
   from latiplanif  -
SUBJECT 'X0323575-PB TECNICA FICH.UOA,TOT Y MVTOS CIERRE %%F1' -
   msgDD MENSAJE -
FILEDD
//*
//*
//*********************************************************************
// INCLUDE MEMBER=FINALJOB
//*********************************************************************
//
