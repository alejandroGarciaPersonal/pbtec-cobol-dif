      *RETRIEVAL.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. B350JI21.
      ******************************************************************00080099
      * ANALISTA   :  NFQ                                              *00090099
      *                                                                *00100099
      * PROGRAMADOR:  NFQ                                              *00110099
      *                                                                *00120099
      * FECHA: 01/12/2020                                              *00130099
      ******************************************************************00140099
      * DESCRIPCION: TRATAMIENTO UMIC                                  *00150099
      *                                                                *00180099
      *    FICHEROS ENTRADA:                                           *00190099
      *             FICHERO 1: CARTERA COLECTIVOS                      *00200099
      *             FICHERO 2: FICHERO DE MOVIMIENTOS                  *00200099
      *             FICHERO 3: TABLA DE BAREMOS.                       *00200099
      *    FICHEROS SALIDA:                                            *00220099
      *             FICHERO 4: PB TECNICA                              *00230099
      *             FICHERO 5: INCIDENCIAS                             *00230099
      ******************************************************************00330099
      * DESCRIPCION: INCIDENCIA PRIMAS CUANDO BONO NO RENOVADO         *00150099
      *                                                <MARCA>230425   *00180099
      *    SE SOLUCIONA ERROR EN CODIGO, QUE TOMABA LA FECHA HASTA DEL *00190099
      *    PERIODO DEL BONO CUANDO SE ACCEDE POR LLAMADA GRUPO EMPRESA *00200099
      ******************************************************************00330099
      * DESCRIPCION: INCIDENCIA REPARTO BONO FUTURO                    *00150099
      *                                                <MARCA>2404     *00180099
      *    SE SOLUCIONA REPARTO   SINIESTROS CUANDO EL BONO ES A       *00190099
      *    FUTURO                                                      *00200099
      ******************************************************************
060326*  MODIFICACION: 06.03.2023   - ETIQUETA <060326>                *
060326*                CORRIGE CALCULA CTA RESULTADOS EN PBAN USANDO   *
060326*                EL ACUMULADO EN LA TABLA DE IMPORTES SINIESTROS *
060326*                POR PERIODO                                     *
      ******************************************************************00330099
      *----------------------------------------------------------------*00350099
       ENVIRONMENT DIVISION.                                            00360099
      *----------------------------------------------------------------*00370099
        CONFIGURATION SECTION.                                          00380099
        SPECIAL-NAMES.                                                  00390099
           DECIMAL-POINT IS COMMA.                                      00400099
      *                                                                 00410099
        INPUT-OUTPUT SECTION.                                           00420099
         FILE-CONTROL.                                                  00430099
      *                                                                 00440099
PBTEC        SELECT ENTRADA-FECHA        ASSIGN TO SYS006.              00450099
      *                                                                 00440099
             SELECT ENTRADA-CTEC         ASSIGN TO SYS001.              00450099
      *                                                                 00460099
             SELECT ENTRADA-MOVS         ASSIGN TO SYS002.              00450099
      *                                                                 00460099
LTA          SELECT ENTRADA-MOVS-LTA     ASSIGN TO SYS012.              00450099
      *                                                                 00460099
LTA          SELECT ENTRADA-TASE-LTA     ASSIGN TO SYS013.              00450099
      *                                                                 00460099
LTA          SELECT ENTRADA-TASE-LTP     ASSIGN TO SYS014.              00450099
      *                                                                 00460099
LTA          SELECT ENTRADA-TASE-LTB     ASSIGN TO SYS015.              00450099
      *                                                                 00460099
BONOA        SELECT ENTRADA-ANTERIOR     ASSIGN TO SYS019.              00450099
      *
             SELECT ENTRADA-TASE         ASSIGN TO SYS003.              00450099
      *                                                                 00460099
             SELECT ENTRADA-TASE-NE      ASSIGN TO SYS007.              00450099
      *                                                                 00460099
             SELECT ENTRADA-TASE-CP      ASSIGN TO SYS008.              00450099
      *                                                                 00460099
FICOL        SELECT ENTRADA-FICOL        ASSIGN TO SYS011.              00450099
      *                                                                 00460099
             SELECT SALIDA-PBT           ASSIGN TO SYS004.              00530099
      *                                                                 00550099
             SELECT SALIDA-INC           ASSIGN TO SYS005.              00450099
      *                                                                 00460099
             SELECT SALIDA-TOT           ASSIGN TO SYS010.              00450099
      *
BONOA        SELECT ENTRADA-TASE-ANT     ASSIGN TO SYS020.              00450099
      *                                                                 00460099
       IDMS-CONTROL SECTION.
       PROTOCOL.
           MODE IS BATCH-AUTOSTATUS.
           IDMS-RECORDS MANUAL.
      *----------------------------------------------------------------*00330099
       DATA DIVISION.                                                   00560099
      *----------------------------------------------------------------*00570099
        FILE SECTION.                                                   00590099
      *                                                                 00600099
PBTEC   FD  ENTRADA-FECHA
PBTEC        LABEL RECORDS ARE STANDARD
PBTEC        BLOCK CONTAINS 0 RECORDS
PBTEC        RECORD CONTAINS 80
PBTEC        DATA RECORD IS REG-FECHAS.
PBTEC   01 REG-FECHAS             PIC X(80).
      *                                                                 00600099
        FD ENTRADA-CTEC                                                 00610099
             LABEL RECORDS ARE STANDARD                                 00620099
             BLOCK CONTAINS 0 RECORDS                                   00630099
             RECORD CONTAINS 194
             DATA RECORD IS REG-ENTRADA1.                               00650099
        01 REG-ENTRADA1           PIC X(194).                           00660099
      *                                                                 00670099
        FD ENTRADA-MOVS                                                 00610099
             LABEL RECORDS ARE STANDARD                                 00620099
             BLOCK CONTAINS 0 RECORDS                                   00630099
FECHAS*      RECORD CONTAINS 120
FECHAS       RECORD CONTAINS 150
             DATA RECORD IS REG-ENTRADA2.                               00650099
FECHAS* 01 REG-ENTRADA2           PIC X(120).                           00660099
FECHAS  01 REG-ENTRADA2           PIC X(150).                           00660099
      *                                                                 00670099
LTA     FD ENTRADA-MOVS-LTA                                             00610099
             LABEL RECORDS ARE STANDARD                                 00620099
             BLOCK CONTAINS 0 RECORDS                                   00630099
FECHAS*      RECORD CONTAINS 120
             RECORD CONTAINS 150
             DATA RECORD IS REG-ENTRADA12.                              00650099
FECHAS* 01 REG-ENTRADA12          PIC X(120).                           00660099
        01 REG-ENTRADA12          PIC X(150).                           00660099
      *                                                                 00670099
LTA     FD ENTRADA-TASE-LTA                                             00610099
             LABEL RECORDS ARE STANDARD                                 00620099
             BLOCK CONTAINS 0 RECORDS                                   00630099
             RECORD CONTAINS 550
             DATA RECORD IS REG-ENTRADA13.                              00650099
        01 REG-ENTRADA13          PIC X(550).                           00660099
      *                                                                 00670099
LTA     FD ENTRADA-TASE-LTP                                             00610099
             LABEL RECORDS ARE STANDARD                                 00620099
             BLOCK CONTAINS 0 RECORDS                                   00630099
             RECORD CONTAINS 550
             DATA RECORD IS REG-ENTRADA14.                              00650099
        01 REG-ENTRADA14          PIC X(550).                           00660099
      *                                                                 00670099
LTA     FD ENTRADA-TASE-LTB                                             00610099
             LABEL RECORDS ARE STANDARD                                 00620099
             BLOCK CONTAINS 0 RECORDS                                   00630099
             RECORD CONTAINS 550
             DATA RECORD IS REG-ENTRADA15.                              00650099
        01 REG-ENTRADA15          PIC X(550).                           00660099
      *                                                                 00670099
BONOA   FD ENTRADA-TASE-ANT                                             00610099
             LABEL RECORDS ARE STANDARD                                 00620099
             BLOCK CONTAINS 0 RECORDS                                   00630099
             RECORD CONTAINS 550
             DATA RECORD IS REG-ENTRADA09.                              00650099
        01 REG-ENTRADA09          PIC X(550).                           00660099
      *                                                                 00670099
BONOA   FD ENTRADA-ANTERIOR                                             00610099
             LABEL RECORDS ARE STANDARD                                 00620099
             BLOCK CONTAINS 0 RECORDS                                   00630099
FECHAS*      RECORD CONTAINS 120
             RECORD CONTAINS 150
             DATA RECORD IS REG-ENTRADA19.                              00650099
FECHAS* 01 REG-ENTRADA19          PIC X(120).                           00660099
FECHAS  01 REG-ENTRADA19          PIC X(150).                           00660099
      *                                                                 00670099
        FD ENTRADA-TASE                                                 00610099
             LABEL RECORDS ARE STANDARD                                 00620099
             BLOCK CONTAINS 0 RECORDS                                   00630099
             RECORD CONTAINS 312
             DATA RECORD IS REG-ENTRADA3.                               00650099
        01 REG-ENTRADA3           PIC X(312).                           00660099
      *
        FD ENTRADA-TASE-NE                                              00610099
             LABEL RECORDS ARE STANDARD                                 00620099
             BLOCK CONTAINS 0 RECORDS                                   00630099
             RECORD CONTAINS 178
             DATA RECORD IS REG-ENTRADA4.                               00650099
        01 REG-ENTRADA4           PIC X(178).                           00660099
      *                                                                 00670099
        FD ENTRADA-TASE-CP                                              00610099
             LABEL RECORDS ARE STANDARD                                 00620099
             BLOCK CONTAINS 0 RECORDS                                   00630099
             RECORD CONTAINS 480
             DATA RECORD IS REG-ENTRADA5.                               00650099
        01 REG-ENTRADA5           PIC X(480).                           00660099
      *                                                                 00670099
        FD ENTRADA-FICOL                                                00610099
             LABEL RECORDS ARE STANDARD                                 00620099
             BLOCK CONTAINS 0 RECORDS                                   00630099
             RECORD CONTAINS 1264
             DATA RECORD IS REG-ENTRADA6.                               00650099
        01 REG-ENTRADA6           PIC X(1264).                          00660099
      *                                                                 00670099
        FD SALIDA-PBT                                                   00880099
             LABEL RECORDS ARE STANDARD                                 00890099
             BLOCK CONTAINS 0 RECORDS                                   00900099
             RECORD CONTAINS 1400
             DATA RECORD IS REG-SALIDA1.                                00920099
        01 REG-SALIDA1            PIC X(1400).                          00930099
      *                                                                 00940099
        FD SALIDA-INC                                                   00880099
             LABEL RECORDS ARE STANDARD                                 00890099
             BLOCK CONTAINS 0 RECORDS                                   00900099
             RECORD CONTAINS 800
             DATA RECORD IS REG-SALIDA2.                                00920099
        01 REG-SALIDA2            PIC X(800).                           00930099
      *                                                                 00940099
        FD SALIDA-TOT                                                   00880099
             LABEL RECORDS ARE STANDARD                                 00890099
             BLOCK CONTAINS 0 RECORDS                                   00900099
             RECORD CONTAINS 236
             DATA RECORD IS REG-SALIDA3.                                00920099
        01 REG-SALIDA3            PIC X(236).                           00930099
      *                                                                 00940099
       SCHEMA SECTION.
       DB  SUR03120 WITHIN SC03VIDA.
      *----------------------------------------------------------------*00570099
       WORKING-STORAGE SECTION.                                         00990099
      *----------------------------------------------------------------*00570099
       01 INICIO-WORKING-STORAGE.
          05 FILLER               PIC X(17) VALUE 'INICIO DE WORKING'.
      *
      *----------------------------------------------------------------*
      *                     L I T E R A L E S                          *
      *----------------------------------------------------------------*
          05 WS-FILE-STATUS.                                            01020099
             10 WS-FS-ENTRADA     PIC X(02) VALUE SPACES.               01030099
             10 WS-FS-SALIDA      PIC X(02) VALUE SPACES.               01070099
      *
          05 WS-FECHA-SIS.                                              02400099
             07 WS-FECHA.                                               02410099
                09 WS-ANO         PIC 9(04) VALUE ZEROS.                02420099
                09 WS-MES         PIC 9(02) VALUE ZEROS.                02430099
                09 WS-DIA         PIC 9(02) VALUE ZEROS.                02440099
             07 WS-HORA.                                                02450099
                09 WS-HH          PIC 9(02) VALUE ZEROS.                02460099
                09 WS-MIN         PIC 9(02) VALUE ZEROS.                02470099
                09 WS-SEG         PIC 9(02) VALUE ZEROS.                02480099
          05 WS-FECHA-FORMATEADA.                                       02400099
             07 WS-DIA-FORMAT     PIC X(02).
             07 FILLER            PIC X(01) VALUE '-'.
             07 WS-MES-FORMAT     PIC X(02).
             07 FILLER            PIC X(01) VALUE '-'.
             07 WS-ANIO-FORMAT    PIC X(04).
          05 WS-FECHA-CIERRE.                                           02400099
             07 WS-DIA-CIERRE     PIC X(02).
             07 FILLER            PIC X(01) VALUE '-'.
             07 WS-MES-CIERRE     PIC X(02).
             07 FILLER            PIC X(01) VALUE '-'.
             07 WS-ANIO-CIERRE    PIC X(04).
          05 WS-FECHA-CIERREF.                                          02400099
             07 WS-DIA-CIERREF    PIC X(02).
             07 WS-MES-CIERREF    PIC X(02).
             07 WS-ANIO-CIERREF   PIC X(04).
          05 WS-FECHA-AUX-ALF.                                          02400099
             07 WS-ANIO-AUXAL     PIC 9(04).
             07 WS-MES-AUXAL      PIC 9(02).
             07 WS-DIA-AUXAL      PIC 9(02).
          05 WS-FECHA-AUX-CNTL.                                         02400099
             07 WS-ANIO-CNTL      PIC 9(04).
             07 WS-MES-CNTL       PIC 9(02).
             07 WS-DIA-CNTL       PIC 9(02).
          05 WS-FECHA-PRIMA-EMPRESA.                                    02400099
             07 WS-ANIO-PRIMA-EMP PIC 9(04).
             07 WS-MES-PRIMA-EMP  PIC 9(02).
             07 WS-DIA-PRIMA-EMP  PIC 9(02).
LTA       05 DATOS-AUX-LTP.
             10 WS-FEC-AUX-LTP-D.
                15 WS-FEC-AUX-LTP-DA        PIC  X(04).
                15 WS-FEC-AUX-LTP-DM        PIC  X(02).
                15 WS-FEC-AUX-LTP-DD        PIC  X(02).
             10 WS-FEC-AUX-LTP-H.
                15 WS-FEC-AUX-LTP-HA        PIC  X(04).
                15 WS-FEC-AUX-LTP-HM        PIC  X(02).
LTA             15 WS-FEC-AUX-LTP-HD        PIC  X(02).
LTA       05 AUX-ANNO-LTP                   PIC  9(04).
      *
SINUSO*MF00105 DATOS-CTEC.
SINUSO*MF001 10 CT-POLIZA-CAR     PIC X(10).
SINUSO*MF001 10 CT-FEFECTO.
SINUSO*MF001    15 CT-FEFECTO-A   PIC X(04).
SINUSO*MF001    15 CT-FEFECTO-M   PIC X(02).
SINUSO*MF001    15 CT-FEFECTO-D   PIC X(02).
SINUSO*MF001 10 CT-FRENOVA-DESDE.
SINUSO*MF001    15 CT-FRENOVA-DDA PIC X(04).
SINUSO*MF001    15 CT-FRENOVA-DDM PIC X(02).
SINUSO*MF001    15 CT-FRENOVA-DDD PIC X(02).
SINUSO*MF001 10 CT-FRENOVA-HASTA.
SINUSO*MF001    15 CT-FRENOVA-HTA PIC X(04).
SINUSO*MF001    15 CT-FRENOVA-HTM PIC X(02).
SINUSO*MF001    15 CT-FRENOVA-HTD PIC X(02).
      *
          05 WS-CADENA            PIC X(08) VALUE 'X0323575'.
      *
MF001     05 I1                             PIC 9(02) VALUE ZEROS.
      *
MF001     05 TB-ACUMULADOS-SINIESTROS.
MF001        10 TB-RENOVADA                 PIC X(01) VALUE SPACES.
MF001        10 TB-RENOVA-DESDE             PIC 9(08) VALUE ZEROS.
MF001        10 TB-RENOVA-HASTA             PIC 9(08) VALUE ZEROS.
MF001        10 TB-DATOS-PERIODO OCCURS 6.
MF001           15 TB-PERIODO               PIC X(05) VALUE SPACES.
MF001           15 TB-FDESDE.
MF001              20 TB-FADESDE            PIC 9(04) VALUE ZEROS.
MF001              20 TB-FMDESDE            PIC 9(02) VALUE ZEROS.
MF001              20 TB-FDDESDE            PIC 9(02) VALUE ZEROS.
MF001           15 TB-FHASTA.
MF001              20 TB-FAHASTA            PIC 9(04) VALUE ZEROS.
MF001              20 TB-FMHASTA            PIC 9(02) VALUE ZEROS.
MF001              20 TB-FDHASTA            PIC 9(02) VALUE ZEROS.
MF001           15 TB-ACUMULADO-206         PIC S9(13)V9(5) VALUE ZEROS.
      *
          05 WS-AUXILIAR.
             10  AUX-PANTICIPO-S            PIC 9(03)V9(02).
             10  AUX-CCUADROBAREMO-S        PIC X(04)      VALUE SPACES.
             10  AUX-PCALCULOPB-S           PIC 9(03)V9(02).
             10  AUX-PGASTOS-S              PIC 9(03)V9(02).
             10  AUX-REAJUSTE-PB            PIC X(01).
LTA          10  AUX-REAJUSTE-LTP           PIC S9(13)V9(5) VALUE ZEROS.
LTA          10  AUX-REAJUSTE-LTB           PIC S9(13)V9(5) VALUE ZEROS.
LTA          10  AUX-IMPORTE-LTB-PBP        PIC S9(13)V9(5) VALUE ZEROS.
LTA          10  AUX-IMPORTE-PBP            PIC S9(13)V9(5) VALUE ZEROS.
LTA          10  AUX-IMPORTE-PBP-ANT        PIC S9(13)V9(5) VALUE ZEROS.
             10  AUX-TIP-REAJUSTE           PIC X(01).
             10  AUX-NANIOPER               PIC 9(02).
             10  AUX-NANIOBEN               PIC 9(02).
             10  AUX-PB-TECNICA             PIC S9(11)V9(2) VALUE ZEROS.
             10  AUX-DEV-NETCOASE           PIC S9(11)V9(2) VALUE ZEROS.
             10  AUX-PCOASE                 PIC 9(03)V9(02).
             10  AUX-LISTABAREMO            PIC 9(3).
             10  AUX-CCOBON-REAJUS          PIC X(01).
             10  AUX-LISTABAREMO-E         OCCURS 120.
                 15  AUX-NASEGURADOSDESDE       PIC 9(6).
                 15  AUX-NASEGURADOSHASTA       PIC 9(6).
                 15  AUX-PPORCENTAJEPB          PIC 9(3)V9(2).
             10  AUX-PPORCEN-REAL           PIC 9(3)V9(2).
             10 WS-MODALIDAD-ORIGEN         PIC X(03).
             10 WS-REAJREC                  PIC X(01) VALUE SPACES.
             10 WS-RENOVADA                 PIC X(01) VALUE SPACES.
290524       10 WS-RENOVADA-SINI            PIC X(01) VALUE SPACES.
151124       10 WS-RENOVADA-ALFA            PIC X(01) VALUE SPACES.
PLURIN       10 WS-RENOVADA-LTA             PIC X(01) VALUE SPACES.
3-PRO        10 WS-AUX-ASEGURADOS           PIC 9(07) VALUE ZEROS.
LTA       05 WS-AUXILIAR-LTA.
             10 WS-FECHA-LTA-AUX.
                15 WS-WSNO-LTA-AUX            PIC 9(04).
                15 WS-MES-LTA-AUX             PIC 9(02).
                15 WS-DIA-LTA-AUX             PIC 9(02).
             10 WS-FECHA-LTA-D.
                15 WS-ANNO-LTA-D              PIC 9(04).
                15 WS-MES-LTA-D               PIC 9(02).
                15 WS-DIA-LTA-D               PIC 9(02).
             10 WS-FECHA-LTA-H.
                15 WS-ANNO-LTA-H              PIC 9(04).
                15 WS-MES-LTA-H               PIC 9(02).
                15 WS-DIA-LTA-H               PIC 9(02).
PLURIN       10 WS-FECHA-LTA-H-REAL.
PLURIN          15 WS-ANNO-LTA-H-REAL         PIC 9(04).
PLURIN          15 WS-MES-LTA-H-REAL          PIC 9(02).
PLURIN          15 WS-DIA-LTA-H-REAL          PIC 9(02).
             10 WS-FECHA-DESDE-ALFA-LTA.
                15 WS-ANNO-DESDE-LTA-H        PIC 9(04).
                15 WS-MES-DESDE-LTA-H         PIC 9(02).
                15 WS-DIA-DESDE-LTA-H         PIC 9(02).
             10 WS-FECHA-HASTA-ALFA-LTA.
                15 WS-ANNO-HASTA-LTA-H        PIC 9(04).
                15 WS-MES-HASTA-LTA-H         PIC 9(02).
                15 WS-DIA-HASTA-LTA-H         PIC 9(02).
LTA          10 AUX-GASTOS-LTA                PIC S9(13)V9(5).
210524       10 WS-PRREAPB-PS-ACTUAL          PIC 9(3)V9(2).
210524       10 WS-PGASTOS-PS-ACTUAL          PIC 9(3)V9(2).
210524       10 WS-PRREAPB-PS-ANTERIOR        PIC 9(3)V9(2).
210524       10 WS-PGASTOS-PS-ANTERIOR        PIC 9(3)V9(2).
PLURIN       10 WS-PRREAPB-PS-FUTURO          PIC 9(3)V9(2).
PLURIN       10 WS-PGASTOS-PS-FUTURO          PIC 9(3)V9(2).
210524       10 WS-PRREAPB-BS-ACTUAL          PIC 9(3)V9(2).
210524       10 WS-PGASTOS-BS-ACTUAL          PIC 9(3)V9(2).
210524       10 WS-PRREAPB-BS-ANTERIOR        PIC 9(3)V9(2).
210524       10 WS-PGASTOS-BS-ANTERIOR        PIC 9(3)V9(2).
PLURIN       10 WS-PRREAPB-BS-FUTURO          PIC 9(3)V9(2).
PLURIN       10 WS-PGASTOS-BS-FUTURO          PIC 9(3)V9(2).
130624       10 WS-KGRUPEMP-PS-ACTUAL         PIC  X(05).
130624       10 WS-KGRUPEMP-BS-ACTUAL         PIC  X(05).
130624       10 WS-KGRUPEMP-PS-ANTERIOR       PIC  X(05).
130624       10 WS-KGRUPEMP-BS-ANTERIOR       PIC  X(05).
PLURIN       10 WS-KGRUPEMP-PS-FUTURO         PIC  X(05).
PLURIN       10 WS-KGRUPEMP-BS-FUTURO         PIC  X(05).
          05 WS-INDICES.
             10  XN-J                       PIC 9(04) VALUE ZEROES.
BONOA     05 WS-FECHA-DESDE-TRAMO.
             10 WS-ANNO-DESDE-TRAMO      PIC 9(04).
             10 WS-MES-DESDE-TRAMO       PIC 9(02).
             10 WS-DIA-DESDE-TRAMO       PIC 9(02).

          05 WS-FECHA-HASTA-TRAMO.
             10 WS-ANNO-HASTA-TRAMO      PIC 9(04).
             10 WS-MES-HASTA-TRAMO       PIC 9(02).
             10 WS-DIA-HASTA-TRAMO       PIC 9(02).

          05 WS-FECHA-DESDE-TRAMO-ANT.
             10 WS-ANNO-DESDE-TRAMO-ANT  PIC 9(04).
             10 WS-MES-DESDE-TRAMO-ANT   PIC 9(02).
             10 WS-DIA-DESDE-TRAMO-ANT   PIC 9(02).

          05 WS-FECHA-HASTA-TRAMO-ANT.
             10 WS-ANNO-HASTA-TRAMO-ANT  PIC 9(04).
             10 WS-MES-HASTA-TRAMO-ANT   PIC 9(02).
             10 WS-DIA-HASTA-TRAMO-ANT   PIC 9(02).

PLURIN    05 WS-TRAMO-FUTSDE-TRAMO-FUT.
PLURIN       10 WS-ANNO-DESDE-TRAMO-FUT  PIC 9(04).
PLURIN       10 WS-MES-DESDE-TRAMO-FUT   PIC 9(02).
PLURIN       10 WS-DIA-DESDE-TRAMO-FUT   PIC 9(02).

PLURIN    05 WS-FECHA-HASTA-TRAMO-FUT.
PLURIN       10 WS-ANNO-HASTA-TRAMO-FUT  PIC 9(04).
PLURIN       10 WS-MES-HASTA-TRAMO-FUT   PIC 9(02).
PLURIN       10 WS-DIA-HASTA-TRAMO-FUT   PIC 9(02).

PLURIN    05 WS-FRENO-DESDE-PLURI.
PLURIN       10 WS-FRENO-DESDE-APLURI     PIC 9(04).
PLURIN       10 WS-FRENO-DESDE-MPLURI     PIC 9(02).
PLURIN       10 WS-FRENO-DESDE-DPLURI       PIC 9(02).

PLURIN    05 WS-FRENO-DESDE-PLURI-ACTUAL.
PLURIN       10 WS-FRENO-DESDE-APLURI-ACTUAL     PIC 9(04).
PLURIN       10 WS-FRENO-DESDE-MPLURI-ACTUAL     PIC 9(02).
PLURIN       10 WS-FRENO-DESDE-DPLURI-ACTUAL     PIC 9(02).

PLURIN    05 WS-FRENO-DESDE-BONO-ACTUAL.
PLURIN       10 WS-FRENO-DESDE-ABONO-ACTUAL     PIC 9(04).
PLURIN       10 WS-FRENO-DESDE-MBONO-ACTUAL     PIC 9(02).
PLURIN       10 WS-FRENO-DESDE-DBONO-ACTUAL     PIC 9(02).

PLURIN    05 WS-FRENO-HASTA-PLURI-ACTUAL.
PLURIN       10 WS-FRENO-HASTA-APLURI-ACTUAL     PIC 9(04).
PLURIN       10 WS-FRENO-HASTA-MPLURI-ACTUAL     PIC 9(02).
PLURIN       10 WS-FRENO-HASTA-DPLURI-ACTUAL     PIC 9(02).

PLURIN    05 WS-FRENO-HASTA-BONO-ACTUAL.
PLURIN       10 WS-FRENO-HASTA-ABONO-ACTUAL     PIC 9(04).
PLURIN       10 WS-FRENO-HASTA-MBONO-ACTUAL     PIC 9(02).
PLURIN       10 WS-FRENO-HASTA-DBONO-ACTUAL     PIC 9(02).

PLURIN    05 WS-FRENO-HASTA-PLURI.
PLURIN       10 WS-FRENO-HASTA-APLURI     PIC 9(04).
PLURIN       10 WS-FRENO-HASTA-MPLURI     PIC 9(02).
PLURIN       10 WS-FRENO-HASTA-DPLURI     PIC 9(02).

RF003     05 WS-FRENO-DESDE-BONO-PBPAG           PIC 9(08).

RF003     05 WS-FRENO-HASTA-BONO-PBPAG           PIC 9(08).

RF003     05 WS-FRENO-DESDE-BONOA-PBPAG          PIC 9(08).

RF003     05 WS-FRENO-HASTA-BONOA-PBPAG          PIC 9(08).

110624    05 WS-FECHA-ALFA.
110624       10 WS-ANNO-FECHA-ALFA       PIC 9(04).
110624       10 WS-MES-FECHA-ALFA        PIC 9(02).
110624       10 WS-DIA-FECHA-ALFA        PIC 9(02).
      *----------------------------------------------------------------*
      *                      S W I C H T S                             *
      *----------------------------------------------------------------*
          05 SW-SWITCH.                                                 01100099
             10 SW-FIN-CAR-TEC    PIC X     VALUE SPACES.               01120099
                88 SW-SI-FINAL-CTEC         VALUE 'S'.                  01130099
                88 SW-NO-FINAL-CTEC         VALUE 'N'.                  01140099
      *                                                                 02390099
             10 SW-FIN-MOVS       PIC X     VALUE SPACES.               01120099
                88 SW-SI-FINAL-MOVS         VALUE 'S'.                  01130099
                88 SW-NO-FINAL-MOVS         VALUE 'N'.                  01140099
      *                                                                 02390099
LTA          10 SW-FIN-MOVS-LTA   PIC X     VALUE SPACES.               01120099
                88 SW-SI-FINAL-MOVS-LTA     VALUE 'S'.                  01130099
                88 SW-NO-FINAL-MOVS-LTA     VALUE 'N'.                  01140099
      *                                                                 02390099
             10 SW-FIN-TASE       PIC X     VALUE SPACES.               01120099
                88 SW-SI-FINAL-TASE         VALUE 'S'.                  01130099
                88 SW-NO-FINAL-TASE         VALUE 'N'.                  01140099

             10 SW-FIN-TASE-NE    PIC X     VALUE SPACES.               01120099
                88 SW-SI-FINAL-TASE-NE      VALUE 'S'.                  01130099
                88 SW-NO-FINAL-TASE-NE      VALUE 'N'.                  01140099

             10 SW-FIN-TASE-CP    PIC X     VALUE SPACES.               01120099
                88 SW-SI-FINAL-TASE-CP      VALUE 'S'.                  01130099
                88 SW-NO-FINAL-TASE-CP      VALUE 'N'.                  01140099

LTA          10 SW-FIN-TASE-LTA   PIC X     VALUE SPACES.               01120099
                88 SW-SI-FINAL-TASE-LTA     VALUE 'S'.                  01130099
                88 SW-NO-FINAL-TASE-LTA     VALUE 'N'.                  01140099

LTA          10 SW-FIN-TASE-LTP   PIC X     VALUE SPACES.               01120099
                88 SW-SI-FINAL-TASE-LTP     VALUE 'S'.                  01130099
                88 SW-NO-FINAL-TASE-LTP     VALUE 'N'.                  01140099

LTA          10 SW-FIN-TASE-LTB   PIC X     VALUE SPACES.               01120099
                88 SW-SI-FINAL-TASE-LTB     VALUE 'S'.                  01130099
                88 SW-NO-FINAL-TASE-LTB     VALUE 'N'.                  01140099

LTA          10 SW-FIN-TASE-ANT   PIC X     VALUE SPACES.               01120099
                88 SW-SI-FINAL-TASE-ANT     VALUE 'S'.                  01130099
                88 SW-NO-FINAL-TASE-ANT     VALUE 'N'.                  01140099

BONOA        10 SW-FIN-BONO-ANT   PIC X     VALUE SPACES.               01120099
                88 SW-SI-FINAL-BONO-ANT     VALUE 'S'.                  01130099
                88 SW-NO-FINAL-BONO-ANT     VALUE 'N'.                  01140099
      *                                                                 02390099
PLURA        10 SW-FIN-PLUR-ANT   PIC X     VALUE SPACES.               01120099
                88 SW-SI-FINAL-PLUR-ANT     VALUE 'S'.                  01130099
                88 SW-NO-FINAL-PLUR-ANT     VALUE 'N'.                  01140099
      *                                                                 02390099
FICOL        10 SW-FIN-FICOL      PIC X     VALUE SPACES.               01120099
FICOL           88 SW-SI-FINAL-FICOL        VALUE 'S'.                  01130099
FICOL           88 SW-NO-FINAL-FICOL        VALUE 'N'.                  01140099

             10 SW-PRIMERA-VEZ    PIC X     VALUE 'S'.
                88 SW-SI-PRIMERA-VEZ        VALUE 'S'.
                88 SW-NO-PRIMERA-VEZ        VALUE 'N'.

             10 SW-ENCONTRADO     PIC X     VALUE 'S'.
                88 SW-SI-ENCONTRADO         VALUE 'S'.
                88 SW-NO-ENCONTRADO         VALUE 'N'.

             10 SW-ERROR          PIC X     VALUE 'N'.
                88 SW-SI-ERROR              VALUE 'S'.
                88 SW-NO-ERROR              VALUE 'N'.

             10  SW-CTRL-631      PIC X     VALUE 'N'.
                 88 FIN-631                 VALUE 'S'.

PBTEC        10  HAY-RB03130      PIC X     VALUE 'S'.
PBTEC        10  HAY-RB03131      PIC X     VALUE 'S'.

PBTEC        10 SW-ENTRADA-FECHAS PIC X     VALUE SPACES.
PBTEC           88 SI-ENTRADA-FECHAS        VALUE 'N'.
PBTEC           88 FIN-ENTRADA-FECHAS       VALUE 'S'.

LTA          10 SW-SW-LTP         PIC X     VALUE SPACES.
LTA             88 SW-NO-LTP                VALUE 'N'.
LTA             88 SW-SI-LTP                VALUE 'S'.

LTA          10 SW-SW-LTB         PIC X     VALUE SPACES.
LTA             88 SW-NO-LTB                VALUE 'N'.
LTA             88 SW-SI-LTB                VALUE 'S'.

BONOA        10 SW-SW-LTPA        PIC X     VALUE SPACES.
                88 SW-NO-LTPA               VALUE 'N'.
                88 SW-SI-LTPA               VALUE 'S'.

             10 SW-SW-LTBA        PIC X     VALUE SPACES.
                88 SW-NO-LTBA               VALUE 'N'.
BONOA           88 SW-SI-LTBA               VALUE 'S'.

RF001        10 SW-SW-LTA-SOLO-ANT-P    PIC X     VALUE SPACES.
RF001           88 SW-NO-LTA-SOLO-ANT-P     VALUE 'N'.
RF001           88 SW-SI-LTA-SOLO-ANT-P     VALUE 'S'.

RF001        10 SW-SW-LTA-SOLO-ANT-B    PIC X     VALUE SPACES.
RF001           88 SW-NO-LTA-SOLO-ANT-B     VALUE 'N'.
RF001           88 SW-SI-LTA-SOLO-ANT-B     VALUE 'S'.

PLURIN       10 SW-SW-LTPF        PIC X     VALUE SPACES.
PLURIN          88 SW-NO-LTPF               VALUE 'N'.
PLURIN          88 SW-SI-LTPF               VALUE 'S'.


PLURIN       10 SW-SW-LTBF        PIC X     VALUE SPACES.
PLURIN          88 SW-NO-LTBF               VALUE 'N'.
PLURIN          88 SW-SI-LTBF               VALUE 'S'.

PLURIN       10 SW-SW-LTA-PERIODO-MITAD PIC X     VALUE SPACES.
PLURIN          88 SW-NO-LTA-MITAD          VALUE 'N'.
PLURIN          88 SW-SI-LTA-MITAD          VALUE 'S'.

PLURIN       10 SW-SW-LTA-PERIODO-FINAL PIC X     VALUE SPACES.
PLURIN          88 SW-NO-LTA-FINAL          VALUE 'N'.
PLURIN          88 SW-SI-LTA-FINAL          VALUE 'S'.
      *                                                                 02390099
MF001        10 SW-PERIODO        PIC X     VALUE SPACES.               01120099
MF001           88 SW-SI-PERIODO            VALUE 'S'.                  01130099
MF001           88 SW-NO-PERIODO            VALUE 'N'.                  01140099
                                                                        01120099
MF001        10 SW-PERIODO-B      PIC X     VALUE SPACES.               01120099
MF001           88 SW-SI-PERIODO-B          VALUE 'S'.                  01130099
MF001           88 SW-NO-PERIODO-B          VALUE 'N'.                  01140099
      *                                                                 02390099
      *----------------------------------------------------------------*
      *                         C O P Y S                              *
      *----------------------------------------------------------------*
      * COPY DE LA CARTERA TECNICA                                     *
      *----------------------------------------------------------------*
          COPY X350JXX2.
      *
      *----------------------------------------------------------------*
      * COPY DE LA CARTERA TECNICA PB - AUX                            *
      *----------------------------------------------------------------*
          COPY X350JPX3.
      *
      *----------------------------------------------------------------*
      * COPY DE FICHERO TOTAL DE ASEGURADOS                            *
      *----------------------------------------------------------------*
          COPY X350JPX4.
      *
      *----------------------------------------------------------------*
      * COPY DE FICHERO TOTAL DE ASEGURADOS CON PRIMA                  *
      *----------------------------------------------------------------*
      **  COPY X350JPX5.
       01 X350JPX5.                                                     04040099
          05 X350JPX5-MODELO        PIC X(04).
          05 X350JPX5-CANAL         PIC X(02).
          05 X350JPX5-KSG-POLSUB.
             10 X350JPX5-KPOLIZA       PIC 9(07).
             10 X350JPX5-KSUBPOL       PIC 9(04).
          05 X350JPX5-SUSCRIP       PIC X(07).
          05 X350JPX5-FORIGENASUSCRI PIC X(04).
          05 X350JPX5-FORIGENMSUSCRI PIC X(02).
          05 X350JPX5-FORIGENDSUSCRI PIC X(02).
          05 X350JPX5-RAMO          PIC X(03).
          05 X350JPX5-MODALIDAD     PIC X(03).
          05 X350JPX5-CESTADOASEG   PIC X(01).
          05 X350JPX5-FECAANULACION PIC X(04).
          05 X350JPX5-FECMANULACION PIC X(02).
          05 X350JPX5-FECDANULACION PIC X(02).
          05 X350JPX5-MOTIVO        PIC X(02).
          05 X350JPX5-UOACOUNT      PIC X(40).
          05 X350JPX5-PREGRUPO      PIC X(04).
          05 X350JPX5-GAP           PIC X(04).
          05 X350JPX5-CASADO        PIC X(01).
          05 X350JPX5-RENOVADA      PIC X(01).
          05 X350JPX5-FRENO-ADESDE  PIC X(04).
          05 X350JPX5-FRENO-MDESDE  PIC X(02).
          05 X350JPX5-FRENO-DDESDE  PIC X(02).
          05 X350JPX5-FRENO-AHASTA  PIC X(04).
          05 X350JPX5-FRENO-MHASTA  PIC X(02).
          05 X350JPX5-FRENO-DHASTA  PIC X(02).
          05 X350JPX5-FCIERREA      PIC X(04).
          05 X350JPX5-FCIERREM      PIC X(02).
          05 X350JPX5-FCIERRED      PIC X(02).
          05 X350JPX5-FORMAPAGO     PIC 9(01).
          05 X350JPX5-COASEGURO     PIC X(01).
          05 X350JPX5-PGLOBAL       PIC X(10).
          05 X350JPX5-IMNOMINADA    PIC X(01).
          05 X350JPX5-NASEGURADOS   PIC 9(09).
          05 X350JPX5-GEMPRESA      PIC X(03).
          05 X350JPX5-REAJUSTE      PIC X(01).
          05 X350JPX5-REAJUSTE-PB   PIC X(01).
          05 X350JPX5-REAJUSTE-REC  PIC X(01).
          05 X350JPX5-TIP-REAJUSTE  PIC X(01).
          05 X350JPX5-APERDIDAS     PIC 9(02).
          05 X350JPX5-ABENEFICI     PIC 9(02).
          05 X350JPX5-ANTICIPO-PB   PIC X(01).
          05 X350JPX5-P-ANTICIPO    PIC 9(03)V9(02).
          05 X350JPX5-BAREMO        PIC X(4).
          05 X350JPX5-REAJUSTEO     PIC 9(3)V9(2).
          05 X350JPX5-P-REAUSTE-PB  PIC 9(03)V9(02).
          05 X350JPX5-P-PB-APLICAR  PIC 9(03)V9(02).
          05 X350JPX5-P-GASTOS-PB   PIC 9(04).
          05 X350JPX5-PRIMA-ANUAL   PIC S9(11)V9(2).
          05 X350JPX5-PRIMA-RECIBOS PIC S9(13)V9(5).
          05 X350JPX5-APUNTE-RECIBO PIC S9(13)V9(5).
          05 X350JPX5-PRIMA-PERIOD  PIC S9(13)V9(5).
          05 X350JPX5-GASTOS        PIC S9(13)V9(5).
          05 X350JPX5-TOTAL-SINIES  PIC S9(13)V9(5).
          05 X350JPX5-IMP-ARRAS-PERD PIC 9(04).
          05 X350JPX5-ASE-TOT-EMPRESA PIC X(08).
          05 X350JPX5-PRI-TOT-EMPRESA PIC S9(13)V9(5).
          05 X350JPX5-SIN-TOT-EMPRESA PIC S9(13)V9(5).
          05 X350JPX5-GASTOS-TOT-EMPR PIC S9(13)V9(5).
          05 X350JPX5-INGRESOS-GASTOS PIC S9(13)V9(5).
          05 X350JPX5-NETA-COASEGUR PIC S9(13)V9(5).
          05 X350JPX5-PB-TECNICA    PIC S9(13)V9(5).
          05 X350JPX5-DEV-NETCOASE  PIC S9(13)V9(5).
          05 X350JPX5-POLIZA-REF.
             11 X350JPX5-POLIZA-REF1 PIC X(07).
             11 X350JPX5-SEPARADOR  PIC X VALUE '/'.
             11 X350JPX5-SUBPOL-REF1 PIC X(04).
          05 X350JPX5-LTA             PIC X(06).
          05 X350JPX5-PBPAGADA-EMP  PIC S9(13)V9(5).

      *
      *----------------------------------------------------------------*
      * COPY DEL FICHERO DE MOVIMIENTOS                                *
      *----------------------------------------------------------------*
FECHAS*   COPY R350T070.
FECHAS    COPY R350T077.
      *
      *
PBTEC *----------------------------------------------------------------*
PBTEC * COPY DEL FICHERO DE FECHAS                                     *
PBTEC *----------------------------------------------------------------*
PBTEC     COPY R350T000.
PBTEC *
PBTEC *----------------------------------------------------------------*
PBTEC * COPY DEL FICHERO DE ERROR                                      *
PBTEC *----------------------------------------------------------------*
PBTEC     COPY X350J000.
PBTEC *
PBTEC *----------------------------------------------------------------*
PBTEC * COPY DEL FICHERO DE TOTALES                                    *
PBTEC *----------------------------------------------------------------*
PBTEC     COPY X350JPX6.
PBTEC *
FICOL * COPY DEL FICHERO DE FICOL                                      *
FICOL *----------------------------------------------------------------*
FICOL     COPY X350FICO.
FICOL *
      *----------------------------------------------------------------*
      * COPY DE LA RUTINA B340M0N2(RECUPERACION UOA)                   *
      *----------------------------------------------------------------*
       01 WS-X340M0N2.
          COPY X340M0N2.
      *
      *----------------------------------------------------------------*
      * COPY DE LA RUTINA B340M0N4(RECUPERACION EXCEPCIONES UOA)       *
      *----------------------------------------------------------------*
       01 WS-X340M0N4.
          COPY X340M0N4.
      *
      *----------------------------------------------------------------*
      * COPY DE LA RUTINA B340M0N4(RECUPERACION EXCEPCIONES UOA)       *
      *----------------------------------------------------------------*
       01 WS-X880M007.
          COPY X880M007.
      *
      *----------------------------------------------------------------*
      * COPY DE LA RUTINA B340JIE0(RECUPERACION GAP)                   *
      *----------------------------------------------------------------*
       01 WS-X340JIE0.
          COPY X340JIE0.
      *
      *----------------------------------------------------------------*
      * COPY DE LA RUTINA XXXXXXXX(RECUPERACION PB)                    *
      *----------------------------------------------------------------*
       01 WS-X350X098.                                                  04040099
          COPY X350X098.                                                04050099
      *
      *----------------------------------------------------------------*
      * COPY DE LA RUTINA B310N505(RECUPERACION ALFA)                  *
      *----------------------------------------------------------------*
       01 WS-X310N505.
          COPY X310N505.
      *
      *----------------------------------------------------------------*
      * COPY DE LA RUTINA RECUPERACION BAREMO                          *
      *----------------------------------------------------------------*
       01 WS-X520X031.
          COPY X520X031.
      *----------------------------------------------------------------*
      * COPY DE LA RUTINA RECUPERACION REAJUSTE RECIBO
      *----------------------------------------------------------------*
       01 WS-X350M100.
          COPY X350M100.
      *
      *----------------------------------------------------------------*
LTA   * COPY RUTINA LTA
      *----------------------------------------------------------------*
RF001***  WS-X350M004.
RF001***  COPY X350M004.
RF001  01  WS-X350M006.
RF001      COPY X350M006.
      *
      *----------------------------------------------------------------*
      *                   C O N T A D O R E S                          *
      *----------------------------------------------------------------*
          05 CONTADORES.
             10 CT-LEIDOS-CTEC    PIC 9(09) VALUE ZEROS.
      *
      *----------------------------------------------------------------*
      * PRIMERA LINEA DE LA CABECERA DEL FICHERO DE SALIDA.            *
      *----------------------------------------------------------------*
       01 PRIMERA-LINEA             PIC X(93).                          04040099
      *
      *----------------------------------------------------------------*
      * SEGUNDA LINEA DE LA CABECERA DEL FICHERO DE SALIDA.            *
      *----------------------------------------------------------------*
       01 SEGUNDA-LINEA             PIC X(860).                         04040099
      *
      *----------------------------------------------------------------*
      * ESTRUCTURA FICHERO DE SALIDA DE LA PB                          *
      *----------------------------------------------------------------*
       01 RSALIDA.                                                      04040099
          05 RSAL-MODELO            PIC X(04).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-CANAL             PIC X(02).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-POLIZA            PIC 9(07).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-SUBPOLIZA         PIC X(04).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-SUSCRIP           PIC X(07).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-FORIGENASUSCRI    PIC X(04).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-FORIGENMSUSCRI    PIC X(02).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-FORIGENDSUSCRI    PIC X(02).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-RAMO              PIC X(03).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-MODALIDAD         PIC X(03).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-CESTADOASEG       PIC X(01).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-FECAANULACION     PIC X(04).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-FECMANULACION     PIC X(02).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-FECDANULACION     PIC X(02).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-MOTIVO            PIC X(02).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-UOACOUNT          PIC X(40).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-PREGRUPO          PIC X(04).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-GAP               PIC X(04).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-CASADO            PIC X(01).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-I1BTI             PIC ZZZ,ZZZZ.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-RENOVADA          PIC X(01).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-FRENO-ADESDE      PIC X(04).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-FRENO-MDESDE      PIC X(02).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-FRENO-DDESDE      PIC X(02).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-FRENO-AHASTA      PIC X(04).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-FRENO-MHASTA      PIC X(02).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-FRENO-DHASTA      PIC X(02).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-FCIERREA          PIC X(04).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-FCIERREM          PIC X(02).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-FCIERRED          PIC X(02).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-FORMAPAGO         PIC 9(01).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-COASEGURO         PIC X(01).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-PCOASEGURO        PIC 999,99.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-PGLOBAL           PIC X(10).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-IMNOMINADA        PIC X(01).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-NASEGURADOS       PIC 9(09).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-ALFA              PIC ZZZZ,ZZZZZZZZ.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-GEMPRESA          PIC X(03).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-REAJUSTE          PIC X(01).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-REAJUSTE-PB       PIC X(01).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-REAJUSTE-REC      PIC X(01).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-TIP-REAJUSTE      PIC X(01).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-APERDIDAS         PIC 9(02).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-ABENEFICI         PIC 9(02).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-ANTICIPO-PB       PIC X(01).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-P-ANTICIPO        PIC 999,99.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-BAREMO            PIC X(4).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-REAJUSTEO         PIC 999,99.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-P-REAUSTE-PB      PIC 999,99.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-P-PB-APLICAR      PIC 999,99.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-P-GASTOS-PB       PIC 999,99.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-PRIMA-ANUAL       PIC -Z.ZZZ.ZZZ.ZZZ,ZZ.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-PRIMA-RECIBOS     PIC -Z.ZZZ.ZZZ.ZZZ.ZZZ,ZZ.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-APUNTE-RECIBO     PIC -Z.ZZZ.ZZZ.ZZZ.ZZZ,ZZ.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-PRIMA-PERIOD      PIC -Z.ZZZ.ZZZ.ZZZ.ZZZ,ZZ.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-GASTOS            PIC -Z.ZZZ.ZZZ.ZZZ.ZZZ,ZZ.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-TOTAL-SINIES      PIC -Z.ZZZ.ZZZ.ZZZ.ZZZ,ZZ.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-IMP-ARRAS-PERD    PIC 9(04).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-ASE-TOT-EMPRESA   PIC X(08).
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-PRI-TOT-EMPRESA   PIC -Z.ZZZ.ZZZ.ZZZ.ZZZ,ZZ.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-SIN-TOT-EMPRESA   PIC -Z.ZZZ.ZZZ.ZZZ.ZZZ,ZZ.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-GASTOS-TOT-EMPR   PIC -Z.ZZZ.ZZZ.ZZZ.ZZZ,ZZ.
          05 FILLER                 PIC X     VALUE ';'.
      *
LTA       05 RSAL-PBPAGA-TOT-EMPR   PIC -Z.ZZZ.ZZZ.ZZZ.ZZZ,ZZ.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-INGRESOS-GASTOS   PIC -Z.ZZZ.ZZZ.ZZZ.ZZZ,ZZ.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-NETA-COASEGUR     PIC -Z.ZZZ.ZZZ.ZZZ.ZZZ,ZZ.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-PB-TECNICA        PIC -Z.ZZZ.ZZZ.ZZZ.ZZZ,ZZ.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-DEV-NETCOASE      PIC -Z.ZZZ.ZZZ.ZZZ.ZZZ,ZZ.
          05 FILLER                 PIC X     VALUE ';'.
      *
          05 RSAL-POLIZA-REF.
             11 RSAL-POLIZA-REF1    PIC X(07).
             11 RSAL-SEPARADOR      PIC X VALUE '/'.
             11 RSAL-SUBPOL-REF1    PIC X(04).
      *
          05 FILLER                 PIC X     VALUE ';'.
      *
LTA       05 RSAL-LTA               PIC X(06).
          05 FILLER                 PIC X     VALUE ';'.
      *
LTA       05 RSAL-PBPAGA            PIC -Z.ZZZ.ZZZ.ZZZ.ZZZ,ZZ.
          05 FILLER                 PIC X     VALUE ';'.

LUNA      05 RSAL-FECSIN-DESDE      PIC X(08).
LUNA      05 FILLER                 PIC X     VALUE ';'.
LUNA      05 RSAL-FECSIN-HASTA      PIC X(08).
LUNA      05 FILLER                 PIC X     VALUE ';'.
      *
      *
      *   05 RSAL-PROV-MATE-INI     PIC X(08).
      *   05 FILLER                 PIC X     VALUE ';'.
      *
      *   05 RSAL-PROV-MATE-FIN     PIC X(08).
      *   05 FILLER                 PIC X     VALUE ';'.
      *
      *   05 RSAL-TOTAL-RESCATES    PIC X(08).
      *   05 FILLER                 PIC X     VALUE ';'.
      *
      *----------------------------------------------------------------*
      *                       C A M P O S                              *
      *                    C A B E C E R A S                           *
      *----------------------------------------------------------------*
       01 LITERALES.
          05 LT-CABECERA.
             10 MODELO              PIC X(06) VALUE 'MODELO'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 CANAL               PIC X(05) VALUE 'CANAL'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 POLIZA              PIC X(06) VALUE 'POLIZA'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 SUBPOLIZA           PIC X(09) VALUE 'SUBPOLIZA'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 SUSCRIPCIONES       PIC X(13) VALUE 'SUSCRIPCIONES'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 FORIGENASUSCRI      PIC X(14) VALUE 'FORIGENASUSCRI'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 FORIGENMSUSCRI      PIC X(14) VALUE 'FORIGENMSUSCRI'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 FORIGENDSUSCRI      PIC X(14) VALUE 'FORIGENDSUSCRI'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 RAMO                PIC X(04) VALUE 'RAMO'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 MODALIDAD           PIC X(09) VALUE 'MODALIDAD'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 ESTADO              PIC X(06) VALUE 'ESTADO'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 FANULACION-A        PIC X(12) VALUE 'FANULACION-A'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 FANULACION-M        PIC X(12) VALUE 'FANULACION-M'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 FANULACION-D        PIC X(12) VALUE 'FANULACION-D'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 MOTIVO              PIC X(06) VALUE 'MOTIVO'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 UOA                 PIC X(03) VALUE 'UOA'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 PREGRUPO            PIC X(08) VALUE 'PREGRUPO'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 GAP                 PIC X(03) VALUE 'GAP'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 CASADO              PIC X(06) VALUE 'CASADO'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 I1BTI               PIC X(05) VALUE 'I1BTI'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 RENOVADA            PIC X(08) VALUE 'RENOVADA'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 FRENO-ADESDE        PIC X(12) VALUE 'FRENO-ADESDE'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 FRENO-MDESDE        PIC X(12) VALUE 'FRENO-MDESDE'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 FRENO-DDESDE        PIC X(12) VALUE 'FRENO-DDESDE'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 FRENO-AHASTA        PIC X(12) VALUE 'FRENO-AHASTA'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 FRENO-MHASTA        PIC X(12) VALUE 'FRENO-MHASTA'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 FRENO-DHASTA        PIC X(12) VALUE 'FRENO-DHASTA'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 CIERRE-PB-ANIO      PIC X(13) VALUE 'CIERRE_PB_ANO'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 CIERRE-PB-MES       PIC X(13) VALUE 'CIERRE_PB_MES'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 CIERRE-PB-DIA       PIC X(13) VALUE 'CIERRE_PB_DIA'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 FORMA-PAGO          PIC X(13) VALUE 'FORMA DE PAGO'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 COASEGURADO         PIC X(09) VALUE 'COASEGURO'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 P-COASEGURADO       PIC X(11) VALUE '% COASEGURO'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 POLIZA-GLOBAL       PIC X(13) VALUE 'POLIZA GLOBAL'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 INNOMINADA          PIC X(10) VALUE 'INNOMINADA'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 NUM-ASEGURADOS      PIC X(14) VALUE 'NUM ASEGURADOS'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 ALFA                PIC X(04) VALUE 'ALFA'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 GRUPO-EMPRESA       PIC X(13) VALUE 'GRUPO EMPRESA'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 REAJUSTE            PIC X(09) VALUE 'REAJUSTE'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 REAJUSTE-PB         PIC X(11) VALUE 'REAJUSTE PB'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 REAJUSTE-RECIBO     PIC X(15) VALUE 'REAJUSTE RECIBO'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 TIPO-REAJUSTE       PIC X(13) VALUE 'TIPO REAJUSTE'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 ANOS-PERDIDAS       PIC X(13) VALUE 'ANOS PERDIDAS'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 ANOS-BENEFICIOS     PIC X(15) VALUE 'ANOS BENEFICIOS'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 ANTICIPO-PB         PIC X(11) VALUE 'ANTICIPO PB'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 P-ANTICIPO          PIC X(10) VALUE '% ANTICIPO'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 BAREMO              PIC X(06) VALUE 'BAREMO'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 REAJUSTE            PIC X(10) VALUE '% REAJUSTE'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 REAJUSTE-PB         PIC X(13) VALUE '% REAJUSTE PB'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 P-PB-APLICAR        PIC X(12) VALUE '% PB APLICAR'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 P-GASTOS-PB         PIC X(11) VALUE '% GASTOS PB'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 PRIMA-ANUAL         PIC X(11) VALUE 'PRIMA ANUAL'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 PRIMA-RECIBOS       PIC X(13) VALUE 'PRIMA RECIBOS'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 APUNTE-RECIBOS      PIC X(14) VALUE 'APUNTE RECIBOS'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 PRIMA-PERIODIF      PIC X(14) VALUE 'PRIMA PERIODIF'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 GASTOS              PIC X(06) VALUE 'GASTOS'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 TOTAL-SINIESTROS    PIC X(16) VALUE 'TOTAL SINIESTROS'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 ARRASTRE-PERDIDAS   PIC X(17) VALUE 'ARRASTRE PERDIDAS'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 A-EMPRESA           PIC X(09) VALUE 'A_EMPRESA'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 P-EMPRESA           PIC X(09) VALUE 'P_EMPRESA'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 S-EMPRESA           PIC X(09) VALUE 'S_EMPRESA'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 G-EMPRESA           PIC X(09) VALUE 'G_EMPRESA'.
             10 FILLER              PIC X     VALUE ';'.
      *
LTA          10 PB-PAGADA-EMPRESA   PIC X(17) VALUE 'PB_PAGADA_EMPRESA'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 INGRESOS-GASTOS     PIC X(09) VALUE 'CTA RTDOS'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 NETA-COASEGUR       PIC X(17) VALUE 'CTA NETA COASEGUR'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 PB-TECNICA          PIC X(10) VALUE 'PB TECNICA'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 DEV-NETCOASE        PIC X(17) VALUE 'REAJ-DEV-NETCOASE'.
             10 FILLER              PIC X     VALUE ';'.
      *
             10 POLIZA-REF          PIC X(10) VALUE 'POLIZA-REF'.
             10 FILLER              PIC X     VALUE ';'.
      *
LTA          10 TIPO-LTA            PIC X(13) VALUE 'LTA'.
             10 FILLER              PIC X     VALUE ';'.
      *
LTA          10 TIPO-PBPAGA         PIC X(09) VALUE 'PB PAGADA'.
             10 FILLER              PIC X     VALUE ';'.

LUNA         10 FEC-SIN-DESDE       PIC X(21)
                                  VALUE 'FECHA GRABACION DESDE'.
             10 FILLER              PIC X     VALUE ';'.

LUNA         10 FEC-SIN-HASTA       PIC X(25)
                                  VALUE 'FECHA GRABACION HASTA'.
             10 FILLER              PIC X     VALUE ';'.
      *
      *      10 PROV-MATE-INI       PIC X(15) VALUE 'PROV MATEMA INI'.
      *      10 FILLER              PIC X     VALUE ';'.
      *
      *      10 PROV-MATE-FIN       PIC X(15) VALUE 'PROV MATEMA FIN'.
      *      10 FILLER              PIC X     VALUE ';'.
      *
      *      10 TOTAL-RESCATES      PIC X(14) VALUE 'TOTAL RESCATES'.
      *      10 FILLER              PIC X     VALUE ';'.
      *
       01 WS-VARIABLES-WORKING.
          05 WS-CLAVE1.
             10 WS-POLIZA-1         PIC X(07) VALUE SPACES.
             10 WS-SUBPOL-1         PIC 9(04) VALUE ZEROES.

          05 WS-CLAVE1-ANT.
             10 WS-POLIZA-1-ANT     PIC X(07) VALUE SPACES.
             10 WS-SUBPOL-1-ANT     PIC 9(04) VALUE ZEROES.

LTA       05 WS-CLAVE1-LTA.
             10 WS-POLIZA-1-LTA     PIC 9(07) VALUE ZEROES.
             10 WS-SUBPOL-1-LTA     PIC 9(04) VALUE ZEROES.

LTA       05 WS-CLAVE1-ANT-LTA.
             10 WS-POLIZA-1-ANT-LTA PIC X(07) VALUE SPACES.
             10 WS-SUBPOL-1-ANT-LTA PIC 9(04) VALUE ZEROES.

          05 WS-CLAVE2.
             10 WS-POLIZA-2         PIC X(07) VALUE SPACES.
             10 WS-SUBPOL-2         PIC 9(04) VALUE ZEROES.

          05 WS-CLAVE2-ANT.
             10 WS-POLIZA-2-ANT     PIC X(07) VALUE SPACES.
             10 WS-SUBPOL-2-ANT     PIC 9(04) VALUE ZEROES.

          05 WS-CLAVE3.
             10 WS-POLIZA-3         PIC X(07) VALUE SPACES.
             10 WS-SUBPOL-3         PIC 9(04) VALUE ZEROES.

          05 WS-CLAVE3-ANT.
             10 WS-POLIZA-3-ANT     PIC X(07) VALUE SPACES.
             10 WS-SUBPOL-3-ANT     PIC 9(04) VALUE ZEROES.

LTA       05 WS-CLAVE3-LTA.
             10 WS-POLIZA-3-LTA     PIC X(07) VALUE SPACES.
             10 WS-SUBPOL-3-LTA     PIC 9(04) VALUE ZEROES.

LTA       05 WS-CLAVE3-ANT-LTA.
             10 WS-POLIZA-3-ANT-LTA PIC X(07) VALUE SPACES.
             10 WS-SUBPOL-3-ANT-LTA PIC 9(04) VALUE ZEROES.

          05 WS-CLAVE4.
             10 WS-POLIZA-4         PIC X(07) VALUE SPACES.
             10 WS-SUBPOL-4         PIC 9(04) VALUE ZEROES.

          05 WS-CLAVE4-ANT.
             10 WS-POLIZA-4-ANT     PIC X(07) VALUE SPACES.
             10 WS-SUBPOL-4-ANT     PIC 9(04) VALUE ZEROES.

          05 WS-CLAVE5.
             10 WS-POLIZA-5         PIC X(07) VALUE SPACES.
             10 WS-SUBPOL-5         PIC 9(04) VALUE ZEROES.

          05 WS-CLAVE5-ANT.
             10 WS-POLIZA-5-ANT     PIC X(07) VALUE SPACES.
             10 WS-SUBPOL-5-ANT     PIC 9(04) VALUE ZEROES.

LTA       05 WS-CLAVE6.
             10 WS-POLIZA-6         PIC X(07) VALUE SPACES.
             10 WS-SUBPOL-6         PIC 9(04) VALUE ZEROES.

LTA       05 WS-CLAVE6-ANT.
             10 WS-POLIZA-6-ANT     PIC X(07) VALUE SPACES.
             10 WS-SUBPOL-6-ANT     PIC 9(04) VALUE ZEROES.

          05 WS-CLAVE-FICOL.
             10 WS-POLIZA-FICOL     PIC X(07) VALUE SPACES.
             10 WS-SUBPOL-FICOL     PIC 9(04) VALUE ZEROES.

          05 WS-CLAVE-FICOL-ANT.
             10 WS-POLIZA-FICOL-ANT     PIC X(07) VALUE SPACES.
             10 WS-SUBPOL-FICOL-ANT     PIC 9(04) VALUE ZEROES.

          05 WS-IMPORTE-205         PIC S9(13)V9(5)
             VALUE ZEROES.
          05 WS-IMPORTE-205-NR      PIC S9(13)V9(5)
             VALUE ZEROES.
JOSENE    05 WS-IMPORTE-205-BONO    PIC S9(13)V9(5)
             VALUE ZEROES.
020924    05 WS-IMPORTE-205-BONO-NO-RENO PIC S9(13)V9(5)
             VALUE ZEROES.
130824    05 WS-IMPORTE-902-BONO    PIC S9(13)V9(5)
             VALUE ZEROES.
          05 WS-IMPORTE-206         PIC S9(13)V9(5)
             VALUE ZEROES.
          05 WS-IMPORTE-206-PLU-BON PIC S9(13)V9(5)
             VALUE ZEROES.
          05 WS-IMPORTE-206-SINI    PIC S9(13)V9(5)
             VALUE ZEROES.
          05 WS-IMPORTE-902         PIC S9(13)V9(5)
             VALUE ZEROES.
          05 WS-IMPORTE-902-NR      PIC S9(13)V9(5)
             VALUE ZEROES.
          05 WS-IMPORTE-206-NR      PIC S9(13)V9(5)
             VALUE ZEROES.
          05 WS-IMPORTE-PERIOD      PIC S9(13)V9(5)
             VALUE ZEROES.
PLURIN    05 WS-IMPORTE-PERIOD-BONO PIC S9(13)V9(5)
PLURIN       VALUE ZEROES.
PLURIN    05 WS-IMPORTE-PERIOD-BONO-ANT PIC S9(13)V9(5)
PLURIN       VALUE ZEROES.
LTA       05 WS-IMPORTE-PERIOD-LTA  PIC S9(13)V9(5)
             VALUE ZEROES.
LTA       05 WS-IMPORTE-PERIOD-PLU-BON  PIC S9(13)V9(5)
             VALUE ZEROES.
LTA       05 WS-IMPORTE-PBP         PIC 9(10)V99 COMP-3
             VALUE ZEROES.
110624    05 WS-IMPORTE-PBP-BONO    PIC 9(10)V99 COMP-3
110624       VALUE ZEROES.
170624    05 WS-IMPORTE-PBP-BONO-FASE2     PIC S9(13)V9(5)
170624                                      VALUE ZEROES.
LTA       05 WS-IMPORTE-PBP-ANT     PIC 9(10)V99 COMP-3
             VALUE ZEROES.
LTA       05 WS-IMPORTE-PBP-X       PIC 9(10)V9(2)
             VALUE ZEROES.
LTA       05 WS-IMPORTE-PBP-AUX     PIC 9(10)V9(2)
             VALUE ZEROES.
          05 WS-IMPORTE-REPARTOS    PIC S9(13)V9(5)
             VALUE ZEROES.
          05 WS-IMPORTE-REPARTOS-NR PIC S9(13)V9(5)
             VALUE ZEROES.
          05 WS-IMPORTE-PERIOD-COA  PIC S9(13)V9(5)
             VALUE ZEROES.
          05 WS-IMPORTE-PLUPE       PIC S9(13)V9(5)
             VALUE ZEROES.
          05 WS-IMPORTE-PLURE       PIC S9(13)V9(5)
             VALUE ZEROES.
          05 WS-IMPORTE-PLUGA       PIC S9(13)V9(5)
             VALUE ZEROES.
LTA       05 AUX-SINIESTROS-LTP     PIC S9(13)V9(5)
             VALUE ZEROES.
LTA       05 AUX-SINIESTROS-LTB     PIC S9(13)V9(5)
             VALUE ZEROES.
LTA       05 WS-IMPORTE-LTB         PIC S9(13)V9(5)
             VALUE ZEROES.
LTA       05 AUX-KGRUPEMP-LTP       PIC X(5) JUST RIGHT.
LTA       05 AUX-KGRUPEMP-LTB       PIC X(5) JUST RIGHT.
LTA       05 WS-IMP-PERI-CALC       PIC S9(13)V9(5)
                                            VALUE ZEROES.
LTA       05 WS-IMP-PERI-CAL1       PIC S9(13)V9(5)
                                            VALUE ZEROES.
LTA       05 WS-IMP-PERI-CAL2       PIC S9(13)V9(5)
                                            VALUE ZEROES.
LTA       05 WS-IMP-PERI-CAL3       PIC S9(13)V9(5)
                                            VALUE ZEROES.

PLURIN    05 WS-EPNETA-NO-RENO-PLURI PIC S9(13)V9(5)
PLURIN                                      VALUE ZEROES.

          05 WS-FEFECTO.
             10 WS-FAEFECTO           PIC X(4)   VALUE SPACES.
             10 WS-NUM-FAEFECTO     REDEFINES
                WS-FAEFECTO           PIC 9(4).
             10 WS-FMEFECTO           PIC X(2)   VALUE SPACES.
             10 WS-FDEFECTO           PIC X(2)   VALUE SPACES.
          05 WS-FCALCULO.
             10 WS-FACALCULO          PIC X(4)   VALUE SPACES.
             10 WS-NUM-FACALCULO    REDEFINES
                WS-FACALCULO          PIC 9(4).
             10 WS-FMCALCULO          PIC X(2)   VALUE SPACES.
             10 WS-FDCALCULO          PIC X(2)   VALUE SPACES.
          05 WS-FPROXRENOV.
             10 WS-FAPROXRENOV        PIC X(4)   VALUE SPACES.
             10 WS-NUM-FAPROXRENOV  REDEFINES
                WS-FAPROXRENOV        PIC 9(4).
             10 WS-FMPROXRENOV        PIC X(2)   VALUE SPACES.
             10 WS-FDPROXRENOV        PIC X(2)   VALUE SPACES.
          05 WS-FORFAIT               PIC X(1)   VALUE SPACES.
          05 WS-ANNO               PIC 9(4)   VALUE ZEROES.
          05 WS-PIC-ANNO         REDEFINES
             WS-ANNO               PIC X(4).

      *VARIABLES PARA EL CALCULO DE DIFERENCIA DE FECHAS C536N002
          05 WS-ENT-C536N002.
             10 WS-DIAINICIO        PIC S9(9) BINARY VALUE ZEROS.
             10 WS-MESINICIO        PIC S9(9) BINARY VALUE ZEROS.
             10 WS-ANNOINICIO       PIC S9(9) BINARY VALUE ZEROS.
             10 WS-DIAFIN           PIC S9(9) BINARY VALUE ZEROS.
             10 WS-MESFIN           PIC S9(9) BINARY VALUE ZEROS.
             10 WS-ANNOFIN          PIC S9(9) BINARY VALUE ZEROS.
             10 WS-CRITERIO         PIC S9(9) BINARY VALUE ZEROS.

      *ESTRUCTURA DE SALIDA GENERICA DE C536N002
          05 WS-SALIDA                 COMP-2 VALUE ZEROS.

          05 WS-FECHA-AUX.
             10 WS-ANNO-AUX           PIC 9(4) VALUE ZEROS.
             10 WS-MES-AUX            PIC 9(2) VALUE ZEROS.
             10 WS-DIA-AUX            PIC 9(2) VALUE ZEROS.

          05 WS-FECHA-NR.
             10 WS-ANNO-NR            PIC 9(4) VALUE ZEROS.
             10 WS-MES-NR             PIC 9(2) VALUE ZEROS.
             10 WS-DIA-NR             PIC 9(2) VALUE ZEROS.

          05 WS-FECHA-DESDE.
             10 WS-ANNO-DESDE         PIC 9(4) VALUE ZEROS.
             10 WS-MES-DESDE          PIC 9(2) VALUE ZEROS.
             10 WS-DIA-DESDE          PIC 9(2) VALUE ZEROS.

          05 WS-FECHA-DESDE-NR.
             10 WS-ANNO-DESNR         PIC 9(4) VALUE ZEROS.
             10 WS-MES-DESNR          PIC 9(2) VALUE ZEROS.
             10 WS-DIA-DESNR          PIC 9(2) VALUE ZEROS.

          05 WS-FECHA-HASTA-NR.
             10 WS-ANNO-HASNR         PIC 9(4) VALUE ZEROS.
             10 WS-MES-HASNR          PIC 9(2) VALUE ZEROS.
             10 WS-DIA-HASNR          PIC 9(2) VALUE ZEROS.

      *ESTRUCTURA PARA EL ARRAY DE INCIDENCIAS EN FORMULAS C
          05 WS-E-INCIDENCIAS.
             10 WS-E-KINCIDENCIA     PIC S9(9) BINARY VALUE ZEROS.
             10 WS-E-K2ENTIDAD       PIC S9(9) BINARY VALUE ZEROS.

PBTEC     05  WS-CLAVE-3130.
PBTEC        10 WS-POLIZA-3130       PIC 9(7)  VALUE ZEROS.
PBTEC        10 WS-SUBPOL-3130       PIC 9(4)  VALUE ZEROS.
PBTEC     05  WS-CLAVE-3131          PIC 9(8)  VALUE ZEROS.

PBTEC     05  WS-CLAVE-3631.
PBTEC        10 WS-POLIZA-3631       PIC 9(7)  VALUE ZEROS.
PBTEC        10 WS-SUBPOL-3631       PIC 9(4)  VALUE ZEROS.
      *RETORNO DE LOS PROGRAMAS DE CALCULO
          05 WS-RETORNO              PIC S9(9) BINARY VALUE ZEROS.

          05 WS-TC                    PIC  9(3)  VALUE ZEROS.
          05 WS-TC-I                  PIC  9(3)V9(2).

          05 WS-ALFA                  PIC 9(4)V9(8)  VALUE ZEROS.
          05 WS-ALFA-PLU-BON          PIC 9(4)V9(8)  VALUE ZEROS.
          05 WS-CONT-REGS             PIC 9(1)       VALUE 1.
          05 WS-SUSCRI-AUX            PIC 9(7)       VALUE ZEROS.
          05 WS-SUSCRI-ANT            PIC 9(7)       VALUE ZEROS.
          05 WS-IMPORTE-SUS           PIC S9(11)V9(2) VALUE ZEROS.
          05 WS-IMPORTE-PRI           PIC S9(11)V9(2) VALUE ZEROS.
          05 WS-NETA-COASEGUR         PIC S9(11)V9(2) VALUE ZEROS.
          05 WS-IMP-GASTOS            PIC S9(11)V9(2) VALUE ZEROS.
LTA       05 WS-IMP-GASTOS-LTA        PIC S9(11)V9(2) VALUE ZEROS.
          05 WS-PRIMA-TOTAL           PIC Z.ZZZ.ZZZ.ZZZ.ZZZ,ZZZZZ.

       01 B340M0N2                  PIC X(08) VALUE 'B340M0N2'.
       01 B340M0N4                  PIC X(08) VALUE 'B340M0N4'.
       01 B880M007                  PIC X(08) VALUE 'B880M007'.
       01 B340JIE0                  PIC X(08) VALUE 'B340JIE0'.
       01 B350M099                  PIC X(08) VALUE 'B350M099'.
       01 C536N002                  PIC X(08) VALUE 'C536N002'.
       01 B310N505                  PIC X(08) VALUE 'B310N505'.
       01 B510M031                  PIC X(08) VALUE 'B510M031'.
       01 B350M100                  PIC X(08) VALUE 'B350M100'.
RF001  01 B350M004                  PIC X(08) VALUE 'B350M006'.
       01 I                         PIC 9(03).
       01 WS-RAMO-ORIGEN.
          05 FILLER                   PIC X(02) VALUE '00'.
          05 WS-RAMO-ORI              PIC X(03) VALUE SPACES.

LUNA      05 WS-FECSIN-DESDE.
LUNA         11 WS-FECSIN-ADESDE    PIC  9(04).
LUNA         11 WS-FECSIN-MDESDE    PIC  9(02).
LUNA         11 WS-FECSIN-DDESDE    PIC  9(02).
LUNA      05 WS-FECSIN-HASTA.
LUNA         11 WS-FECSIN-AHASTA    PIC  9(04).
LUNA         11 WS-FECSIN-MHASTA    PIC  9(02).
LUNA         11 WS-FECSIN-DHASTA    PIC  9(02).
LUNA      05 WS-FECSIN-CIERRE.
LUNA         11 WS-FECSIN-ACIERRE   PIC  9(04).
LUNA         11 WS-FECSIN-MCIERRE   PIC  9(02).
LUNA         11 WS-FECSIN-DCIERRE   PIC  9(02).
LUNA      05 WS-FECSIN-DESDE-PB.
LUNA         11 WS-FECSIN-ADESDEPB  PIC  9(04).
LUNA         11 WS-FECSIN-MDESDEPB  PIC  9(02).
LUNA         11 WS-FECSIN-DDESDEPB  PIC  9(02).
JOSENE    05 WS-FECSIN-DESDE-PLU-BON.
JOSENE       11 WS-FECSIN-ADESDE-PLU-BON     PIC  9(04).
JOSENE       11 WS-FECSIN-MDESDE-PLU-BON     PIC  9(02).
JOSENE       11 WS-FECSIN-DDESDE-PLU-BON     PIC  9(02).
JOSENE    05 WS-FECSIN-HASTA-PLU-BON.
JOSENE       11 WS-FECSIN-AHASTA-PLU-BON     PIC  9(04).
JOSENE       11 WS-FECSIN-MHASTA-PLU-BON     PIC  9(02).
JOSENE       11 WS-FECSIN-DHASTA-PLU-BON     PIC  9(02).
          05 WS-ANTPBAN             PIC  X(01) VALUE 'N'.
      *********************
      * COPYS
      *********************
      *
       COPY IDMS RECORD SUBSCHEMA-CTRL.
      *
       COPY IDMS SUBSCHEMA-SSNAME.
      *
       COPY IDMS SUBSCHEMA-AREANAMES.
      *
       COPY IDMS SUBSCHEMA-SETNAMES.
      *
       COPY IDMS SUBSCHEMA-RECNAMES.
      *
       COPY IDMS RECORD RB03130.
       COPY IDMS RECORD RB03131.
       COPY IDMS RECORD RB03631.
       COPY IDMS RECORD RB03634.
       COPY IDMS RECORD RA861011.
       COPY IDMS RECORD RR861001.
       COPY IDMS RECORD RW86CRC1.
       COPY IDMS RECORD RT54130.
       COPY IDMS RA541086.
      *

       01 FINAL-WORKING-STORAGE.
          05 FILLER               PIC X(16) VALUE 'FINAL DE WORKING'.
      *----------------------------------------------------------------*03850099
      *                      LINKAGE SECTION                           *03860099
      *----------------------------------------------------------------*03870099
      *
      *----------------------------------------------------------------*
      *                                                                *
      *              P R O C E D U R E   D I V I S I O N               *
      *                                                                *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.                                              04510099
      ******************************************************************04520099
           PERFORM 1000-INICIO                                          04540099
      *                                                                 04550099
           PERFORM 2000-PROCESO                                         04560099
             UNTIL SW-SI-FINAL-CTEC
      *                                                                 04590099
      *                                                                 04590099
           PERFORM 9000-FIN                                             04600099
           .
      *
      ******************************************************************04520099
       1000-INICIO.
      ******************************************************************04520099
      *D*
      *    DISPLAY '1000-INICIO'
      *F*
           OPEN  INPUT ENTRADA-CTEC
                       ENTRADA-MOVS
LTA                    ENTRADA-MOVS-LTA
      *                ENTRADA-TASE
ESTEBN                 ENTRADA-FECHA
BONOA *                ENTRADA-ANTERIOR
                OUTPUT SALIDA-PBT
                       SALIDA-INC
                       SALIDA-TOT
      *
           INITIALIZE REG-SALIDA1
                      REG-SALIDA2
                      REG-SALIDA3
      *
PBTEC      INITIALIZE X350JPX3
            REPLACING ALPHANUMERIC BY SPACES
                           NUMERIC BY ZEROES

PBTEC      INITIALIZE X350JPX6
            REPLACING ALPHANUMERIC BY SPACES
                           NUMERIC BY ZEROES

           INITIALIZE RSALIDA

           INITIALIZE WS-X350X098
            REPLACING ALPHANUMERIC BY SPACES
                           NUMERIC BY ZEROES

           MOVE FUNCTION CURRENT-DATE  TO WS-FECHA-SIS
           MOVE WS-DIA         TO WS-DIA-FORMAT
           MOVE WS-MES         TO WS-MES-FORMAT
           MOVE WS-ANO         TO WS-ANIO-FORMAT
      *
           MOVE LT-CABECERA            TO SEGUNDA-LINEA                 04930099
      *
           SET SW-NO-FINAL-CTEC        TO TRUE
           SET SW-NO-FINAL-MOVS        TO TRUE
      *
           PERFORM 1001-ABRIR-AREAS
PBTEC      PERFORM 2704-LEER-FECHA

           PERFORM 3100-LEER-ENTRADA-MOVS
           PERFORM 3200-LEER-ENTRADA-CTEC
LTA        PERFORM 5213-LEER-ENTRADA-MOVS-LTA
BONOA *    PERFORM 5214-LEER-ENTRADA-ANTERIOR
      *
           PERFORM 5100-ESCRIBIR-PRIMERA-LINEA
           PERFORM 5200-ESCRIBIR-SEGUNDA-LINEA
      *
           .
      *
      ******************************************************************04520099
       1001-ABRIR-AREAS.
      ******************************************************************04520099
      *
       COPY IDMS SUBSCHEMA-BINDS.
       READY AR03130 USAGE-MODE IS RETRIEVAL.
       READY AR03XD6 USAGE-MODE IS RETRIEVAL.
       READY AR03631 USAGE-MODE IS RETRIEVAL.
       READY AR03X80 USAGE-MODE IS RETRIEVAL.
       READY AR03X82 USAGE-MODE IS RETRIEVAL.
      *
      ******************************************************************04520099
       2000-PROCESO.
      ******************************************************************04520099
      *B*
      *    DISPLAY '2000-PROCESO POLIZA  ' X350JXX2-KPOLIZA
      *    DISPLAY 'X350JXX2-FRENO-DESDE ' X350JXX2-FRENO-DESDE
      *    DISPLAY 'X350JXX2-FRENO-HASTA ' X350JXX2-FRENO-HASTA
      *    DISPLAY 'R350T000-FFINAL     ' R350T000-FFINAL
      *F*

           IF WS-CLAVE1 = WS-CLAVE2

PBTEC         MOVE WS-CLAVE1        TO WS-CLAVE1-ANT
              MOVE WS-CLAVE2        TO WS-CLAVE2-ANT

              INITIALIZE WS-IMPORTE-205
                         WS-IMPORTE-205-NR
LTA                      WS-IMPORTE-PBP
LTA                      WS-IMPORTE-PBP-ANT
                         WS-IMPORTE-206
                         WS-IMPORTE-206-SINI
                         WS-IMPORTE-902
                         WS-IMPORTE-902-NR
                         WS-IMPORTE-206-NR
                         WS-AUXILIAR
LTA                      WS-IMPORTE-PERIOD-LTA
LTA                      WS-IMP-PERI-CAL1
LTA                      WS-IMP-PERI-CAL2
LTA                      WS-IMP-PERI-CAL3
LTA                      WS-IMP-PERI-CALC
LTA                      WS-ALFA-PLU-BON
170624                   WS-IMPORTE-PBP-BONO-FASE2
200624                   WS-IMPORTE-PBP-ANT
200624                   WS-IMPORTE-PBP-BONO
200624                   X350M004
200624                   WS-FEC-AUX-LTP-D

PLURIN        SET SW-NO-LTA-FINAL         TO TRUE
PLURIN        SET SW-NO-LTA-MITAD         TO TRUE
RF001         SET SW-NO-LTA-SOLO-ANT-B    TO TRUE
RF001         SET SW-NO-LTA-SOLO-ANT-P    TO TRUE
RF001         SET SW-NO-LTP               TO TRUE
RF001         SET SW-NO-LTB               TO TRUE
RF001         SET SW-NO-LTPA              TO TRUE
RF001         SET SW-NO-LTBA              TO TRUE

LTA   *       PERFORM 5200-GESTION-LTA
              PERFORM 5210-LLAMADA-LTP
              PERFORM 5220-LLAMADA-LTB

RF001 *D*
RF001 *    DISPLAY 'ANTES '
RF001 *    DISPLAY 'SW-NO-LTB ' SW-SW-LTB
RF001 *    DISPLAY 'SW-SW-LTBA' SW-SW-LTBA
RF001 *    DISPLAY 'SW-SW-LTP ' SW-SW-LTP
RF001 *    DISPLAY 'SW-SW-LTPA ' SW-SW-LTPA
RF001 *    DISPLAY 'WS-FECHA-LTA-H ' WS-FECHA-LTA-H
RF001 *F*
                   IF X350M004-FDESA-PS-ANT > 0
                      SET SW-SI-LTPA TO TRUE
                   END-IF
                   IF X350M004-FDESA-PS > 0
                      SET SW-SI-LTP  TO TRUE
                   END-IF
RF001              IF X350M004-FDESA-BS-ANT > 0
                      SET SW-SI-LTBA TO TRUE
                   END-IF
                   IF X350M004-FDESA-BS > 0
                      SET SW-SI-LTB  TO TRUE
RF001              END-IF


RF001              IF SW-NO-LTP AND SW-SI-LTPA
RF001 *D*
RF001 *    DISPLAY 'ENTRO IF - PLURI'
RF001 *F*
RF001                  SET SW-NO-LTP TO TRUE
RF001                  SET SW-SI-LTA-SOLO-ANT-P TO TRUE
RF001              END-IF

RF001              IF SW-NO-LTB AND SW-SI-LTBA
RF001 *D*
RF001 *    DISPLAY 'ENTRO IF-BONO'
RF001 *F*
RF001                  SET SW-NO-LTB TO TRUE
RF001                  SET SW-SI-LTA-SOLO-ANT-B TO TRUE
RF001              END-IF
RF001 *D*
RF001 *    DISPLAY 'DESPUES   '
RF001 *    DISPLAY 'SW-SW-LTB ' SW-SW-LTB
RF001 *    DISPLAY 'SW-SW-LTBA' SW-SW-LTBA
RF001 *    DISPLAY 'SW-SW-LTP ' SW-SW-LTP
RF001 *    DISPLAY 'SW-SW-LTPA ' SW-SW-LTPA
RF001 *F*
MF001
MF001      INITIALIZE TB-ACUMULADOS-SINIESTROS
MF001                 REPLACING ALPHANUMERIC DATA BY SPACES
MF001                                NUMERIC DATA BY ZEROES
MF001
MF001
MF001      MOVE X350JXX2-FRENO-DESDE       TO TB-RENOVA-DESDE
MF001      MOVE X350JXX2-FRENO-HASTA       TO TB-RENOVA-HASTA
MF001
MF001      IF X350JXX2-FRENO-HASTA        >  R350T000-FFINAL
MF001         MOVE 'S'                     TO TB-RENOVADA
MF001      ELSE
MF001         MOVE 'N'                     TO TB-RENOVADA
MF001      END-IF
MF001      IF SW-NO-LTP
MF001         IF X350JXX2-FRENO-HASTA   >  R350T000-FFINAL
MF001            MOVE 'PB   '              TO TB-PERIODO (1)
MF001            MOVE TB-RENOVA-DESDE      TO TB-FDESDE  (1)
MF001            MOVE TB-RENOVA-HASTA      TO TB-FHASTA  (1)
MF001         ELSE
MF001            MOVE 'PB   '              TO TB-PERIODO (1)
MF001            MOVE TB-RENOVA-HASTA      TO TB-FDESDE  (1)
MF001                                         TB-FHASTA  (1)
MF001            ADD   1                   TO TB-FAHASTA (1)
MF001
MF001            IF SW-NO-LTPA
MF001               MOVE 'PBAN '           TO TB-PERIODO (2)
MF001               MOVE TB-RENOVA-DESDE   TO TB-FDESDE  (2)
MF001               MOVE TB-RENOVA-HASTA   TO TB-FHASTA  (2)
MF001            END-IF
MF001         END-IF
MF001      END-IF
MF001
MF001      IF SW-SI-LTP
MF001         MOVE 'PLURI'                  TO TB-PERIODO (3)
MF001         MOVE X350M004-FDESA-PS        TO TB-FADESDE (3)
MF001         MOVE X350M004-FDESM-PS        TO TB-FMDESDE (3)
MF001         MOVE X350M004-FDESD-PS        TO TB-FDDESDE (3)
MF001
MF001         MOVE X350M004-FHASA-PS        TO TB-FAHASTA (3)
MF001         MOVE X350M004-FHASM-PS        TO TB-FMHASTA (3)
MF001         MOVE X350M004-FHASD-PS        TO TB-FDHASTA (3)
MF001         IF TB-RENOVA-HASTA  <=  TB-FDESDE (3)
MF001            MOVE 'PBAN '              TO TB-PERIODO (2)
MF001            MOVE TB-RENOVA-DESDE      TO TB-FDESDE  (2)
MF001            MOVE TB-RENOVA-HASTA      TO TB-FHASTA  (2)
MF001         END-IF
MF001      END-IF
MF001
MF001      IF SW-SI-LTPA
MF001           MOVE 'PLURA'                TO TB-PERIODO (4)
MF001           MOVE X350M004-FDESA-PS-ANT  TO TB-FADESDE (4)
MF001           MOVE X350M004-FDESM-PS-ANT  TO TB-FMDESDE (4)
MF001           MOVE X350M004-FDESD-PS-ANT  TO TB-FDDESDE (4)
MF001
MF001           MOVE X350M004-FHASA-PS-ANT  TO TB-FAHASTA (4)
MF001           MOVE X350M004-FHASM-PS-ANT  TO TB-FMHASTA (4)
MF001           MOVE X350M004-FHASD-PS-ANT  TO TB-FDHASTA (4)
MF001      END-IF
MF001
MF001      IF SW-SI-LTB
MF001           MOVE 'BONO '                TO TB-PERIODO (5)
MF001           MOVE X350M004-FDESA-BS      TO TB-FADESDE (5)
MF001           MOVE X350M004-FDESM-BS      TO TB-FMDESDE (5)
MF001           MOVE X350M004-FDESD-BS      TO TB-FDDESDE (5)
MF001
MF001           MOVE X350M004-FHASA-BS      TO TB-FAHASTA (5)
MF001           MOVE X350M004-FHASM-BS      TO TB-FMHASTA (5)
MF001           MOVE X350M004-FHASD-BS      TO TB-FDHASTA (5)
MF001      END-IF
MF001
MF001      IF SW-SI-LTBA
MF001           MOVE 'BONOA'                TO TB-PERIODO (6)
MF001           MOVE X350M004-FDESA-BS-ANT  TO TB-FADESDE (6)
MF001           MOVE X350M004-FDESM-BS-ANT  TO TB-FMDESDE (6)
MF001           MOVE X350M004-FDESD-BS-ANT  TO TB-FDDESDE (6)
MF001
MF001           MOVE X350M004-FHASA-BS-ANT  TO TB-FAHASTA (6)
MF001           MOVE X350M004-FHASM-BS-ANT  TO TB-FMHASTA (6)
MF001           MOVE X350M004-FHASD-BS-ANT  TO TB-FDHASTA (6)
MF001      END-IF
MF001
MF001 *D*
MF001 *    DISPLAY ' PERIODOS POLIZA ' X350JXX2-KPOLIZA ' / '
MF001 *                                X350JXX2-KSUBPOL
MF001 *    DISPLAY ' TB-RENOVA-DESDE ' TB-RENOVA-DESDE
MF001 *    DISPLAY ' TB-RENOVA-HASTA ' TB-RENOVA-HASTA
MF001 *    PERFORM VARYING I1 FROM 1 BY 1 UNTIL I1 > 6
MF001 *      DISPLAY ' TB-PERIODO (' I1 ') ' TB-PERIODO (I1)
MF001 *      DISPLAY ' TB-FDESDE  (' I1 ') ' TB-FDESDE  (I1)
MF001 *           ' /  TB-FHASTA  (' I1 ') ' TB-FHASTA  (I1)
MF001 *    END-PERFORM
MF001 *
MF001 *F*
110624        MOVE R350T000-FFINAL TO WS-FECHA-ALFA
110624        MOVE 01 TO WS-DIA-FECHA-ALFA
110624        MOVE 01 TO WS-MES-FECHA-ALFA

LTA        IF SW-NO-LTP
      *D*
      *       DISPLAY 'NO PASA VALIDACION DE LTA'
      *F*
                PERFORM 2400-ACUMULAR-MOVS
                  UNTIL WS-CLAVE1 NOT EQUAL WS-CLAVE1-ANT

      *       PERFORM 2701-LLAMADA-RB03031
                PERFORM 2702-LLAMADA-FICOL

                PERFORM OBTENER-RB03631-RR

2-PRO           IF AUX-CCUADROBAREMO-S NOT EQUAL SPACES
      *D*
      *    DISPLAY 'AUX-CCUADROBAREMO-S ' AUX-CCUADROBAREMO-S
      *    DISPLAY 'X350JPX4-NASEGURADO ' X350JPX4-NASEGURADO
      *    DISPLAY 'X350JXX2-NASEGURADO ' X350JXX2-NASEGURADO
      *    DISPLAY 'X350JXX2-KGRUEMP    ' X350JXX2-KGRUEMP
      *    DISPLAY 'X350JPX4-KGRUEMP    ' X350JPX4-KGRUEMP
      *F*
3-PRO            IF X350JXX2-KGRUEMP NOT EQUAL SPACES

3-PRO              OPEN INPUT ENTRADA-TASE
3-PRO              PERFORM 2703-LEER-TOTALES-ASEGURADO
3-PRO                UNTIL X350JXX2-KGRUEMP EQUAL
3-PRO                      X350JPX4-KGRUEMP
3-PRO              CLOSE ENTRADA-TASE
3-PRO                MOVE X350JPX4-NASEGURADO TO WS-AUX-ASEGURADOS
3-PRO            ELSE
3-PRO               MOVE X350JXX2-NASEGURADO TO WS-AUX-ASEGURADOS
3-PRO            END-IF
      *D*
3-PRO *          DISPLAY 'WS-AUX-ASEGURADOS ' WS-AUX-ASEGURADOS
      *F*
                 PERFORM 2702-CONSULTAR-BAREMO
2-PRO           END-IF
                PERFORM 2600-LINEA-MVTOS

                PERFORM 5000-GRABAR-SALIDA

                PERFORM 5100-GRABAR-SALIDA-TOT

                PERFORM 2500-TRATAR-SUSCRIP
                UNTIL WS-CLAVE4 NOT EQUAL WS-CLAVE4-ANT

RF001           IF SW-NO-LTB OR SW-NO-LTP
RF001             IF SW-SI-LTA-SOLO-ANT-P
      *D*
      *    DISPLAY 'entro if-SOLO-ANT-P'
      *F*
RF001               PERFORM 5211-GESTION-MOVIMIENTOS-ANT
RF001               PERFORM 5000-GRABAR-SALIDA-LTP-A
RF001                IF SW-SI-LTA-SOLO-ANT-B
RF001                  PERFORM 5000-GRABAR-SALIDA-LTB-A
RF001                END-IF
RF001             END-IF
RF001             IF SW-SI-LTA-SOLO-ANT-B AND SW-NO-LTA-SOLO-ANT-P
      *D*
      *    DISPLAY 'entro if-SOLO-ANT-B'
      *F*
RF001                PERFORM 5211-GESTION-MOVIMIENTOS-ANT
RF001                PERFORM 5000-GRABAR-SALIDA-LTB-A
RF001             END-IF
RF001       END-IF

LTA          ELSE
      *D*
      *       DISPLAY 'PASA VALIDACION DE LTA'
      *F*
                PERFORM 5200-GESTION-LTA
                PERFORM 3200-LEER-ENTRADA-CTEC
PBTEC        END-IF
           END-IF

MF001 *D*                                                               04600099
MF001 *    PERFORM VARYING I1 FROM 1 BY 1 UNTIL I1 > 6                  04600099
MF001 *      DISPLAY ' TB-PERIODO (' I1 ') ' TB-PERIODO (I1)
MF001 *      DISPLAY ' TB-FDESDE  (' I1 ') ' TB-FDESDE  (I1)
MF001 *           ' /  TB-FHASTA  (' I1 ') ' TB-FHASTA  (I1)
MF001 *      DISPLAY ' TB-ACUMULADO-206 ( ' I1 ') '
MF001 *                TB-ACUMULADO-206 (I1)
MF001 *    END-PERFORM                                                  04600099
MF001 *F*                                                               04600099
           IF WS-CLAVE1 < WS-CLAVE2
              PERFORM 3100-LEER-ENTRADA-MOVS
           END-IF

           IF WS-CLAVE1 > WS-CLAVE2
              PERFORM 3200-LEER-ENTRADA-CTEC
           END-IF

           MOVE ZEROES TO WS-IMPORTE-PBP
           .
      *
      ******************************************************************10650099
PBTEC  2100-RECUPERAR-DATOS-SI-RENO.                                    10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2100-RECUPERAR-DATOS-SI-RENO'
      *    DISPLAY 'X350JPX3-KPOLIZA            ' X350JPX3-KPOLIZA
      *    DISPLAY 'X350JPX3-KSUBPOL            ' X350JPX3-KSUBPOL
      *F*

SINUSO*MF001 INITIALIZE DATOS-CTEC
MF001      INITIALIZE
                      RSAL-PRIMA-RECIBOS
                      X350JPX6-PRIMA-RECIBOS
                      RSAL-APUNTE-RECIBO
                      X350JPX6-APUNTE-RECIBO
                      RSAL-TOTAL-SINIES
                      RSAL-INGRESOS-GASTOS
                      X350JPX6-INGRESOS-GASTOS
                      RSAL-NETA-COASEGUR
                      X350JPX6-NETA-COASEGUR
                      RSAL-PB-TECNICA
                      X350JPX6-PB-TECNICA
                      RSAL-DEV-NETCOASE
                      X350JPX6-DEV-NETCOASE
                      WS-IMPORTE-REPARTOS

           MOVE ZEROES TO RSAL-PRIMA-RECIBOS
                          X350JPX6-PRIMA-RECIBOS

           MOVE '3000'                   TO RSAL-MODELO

           MOVE X350JPX3-CCANAL          TO RSAL-CANAL
                                            X350JPX6-CANAL
           MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA
           MOVE X350JPX3-KSUBPOL         TO RSAL-SUBPOLIZA
           MOVE X350JPX3-KSUSCRI         TO RSAL-SUSCRIP

           MOVE X350JPX3-FEFECTO-AINIPOL TO RSAL-FORIGENASUSCRI
           MOVE X350JPX3-FEFECTO-MINIPOL TO RSAL-FORIGENMSUSCRI
           MOVE X350JPX3-FEFECTO-DINIPOL TO RSAL-FORIGENDSUSCRI

           MOVE X350JPX3-KRAMO           TO RSAL-RAMO
                                            X350JPX6-RAMO
           MOVE X350JPX3-MODALI          TO RSAL-MODALIDAD
                                            X350JPX6-MODALIDAD
           MOVE X350JPX3-VIGOR-ANUL      TO RSAL-CESTADOASEG
           MOVE '00'                     TO RSAL-PREGRUPO(1:2)
           MOVE X350JPX3-CARTINV         TO RSAL-PREGRUPO(3:2)
           MOVE X350JPX3-CASADA          TO RSAL-CASADO
           MOVE X350JPX3-PINTER-TECBASE  TO RSAL-I1BTI

           MOVE X350JPX3-FRENO-ADESDE    TO RSAL-FRENO-ADESDE
           MOVE X350JPX3-FRENO-MDESDE    TO RSAL-FRENO-MDESDE
           MOVE X350JPX3-FRENO-DDESDE    TO RSAL-FRENO-DDESDE

           MOVE X350JPX3-FRENO-AHASTA TO RSAL-FRENO-AHASTA
           MOVE X350JPX3-FRENO-MHASTA TO RSAL-FRENO-MHASTA
           MOVE X350JPX3-FRENO-DHASTA TO RSAL-FRENO-DHASTA

           MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
                                            X350JPX6-FCIERREA
           MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
                                            X350JPX6-FCIERREM
           MOVE R350T000-FDFINAL         TO RSAL-FCIERRED
                                            X350JPX6-FCIERRED
LTA        MOVE 'PB'                     TO RSAL-LTA
           MOVE X350JXX2-FCOBRO          TO RSAL-FORMAPAGO
           MOVE X350JPX3-TIPO-COASE      TO RSAL-COASEGURO
           MOVE X350JPX3-PCOASE          TO RSAL-PCOASEGURO
           MOVE X350JPX3-PAGADOR         TO RSAL-PGLOBAL

           IF X350JPX3-CERTI EQUAL '000000' AND                         000
              X350JPX3-ASEG-MECANIZADOS = 'N'                           000
              MOVE 'S' TO RSAL-IMNOMINADA                               000
           ELSE
              MOVE 'N' TO RSAL-IMNOMINADA                               000
           END-IF

           MOVE X350JPX3-NASEGURADO      TO RSAL-NASEGURADOS

           IF X350JPX3-VIGOR-ANUL  = 'A'
              MOVE X350JPX3-ANO-ANUL     TO RSAL-FECAANULACION
              MOVE X350JPX3-MES-ANUL     TO RSAL-FECMANULACION
              MOVE X350JPX3-DIA-ANUL     TO RSAL-FECDANULACION
              MOVE X350JPX3-MOTIVO-ANU   TO RSAL-MOTIVO
           END-IF

           MOVE WS-ALFA                  TO RSAL-ALFA

200524          IF SW-SI-LTB
200524             MOVE X350M004-KGRUPEMP-BS(3:3) TO X350JPX3-KGRUEMP
200524          END-IF

200524          IF X350JPX3-KGRUEMP = ZEROS
200524            MOVE SPACES           TO X350JPX3-KGRUEMP
200524          END-IF
           MOVE X350JPX3-KGRUEMP         TO RSAL-GEMPRESA

PBTECJ*    IF AUX-TIP-REAJUSTE = '1' OR 'T'
      *       MOVE 'S'                      TO RSAL-REAJUSTE
      *    ELSE
      *       MOVE 'N'                      TO RSAL-REAJUSTE
      *    END-IF

           MOVE AUX-CCOBON-REAJUS        TO RSAL-REAJUSTE

           MOVE AUX-REAJUSTE-PB          TO RSAL-REAJUSTE-PB

           MOVE WS-REAJREC               TO RSAL-REAJUSTE-REC
           MOVE AUX-TIP-REAJUSTE         TO RSAL-TIP-REAJUSTE
           MOVE AUX-NANIOPER             TO RSAL-APERDIDAS
           MOVE AUX-NANIOBEN             TO RSAL-ABENEFICI

           IF X350X098-IANTICIPO-S > ZEROES
              MOVE 'S'                   TO RSAL-ANTICIPO-PB
           ELSE
              MOVE 'N'                   TO RSAL-ANTICIPO-PB
           END-IF

           MOVE AUX-PANTICIPO-S          TO RSAL-P-ANTICIPO
           MOVE AUX-CCUADROBAREMO-S      TO RSAL-BAREMO
           MOVE AUX-PCALCULOPB-S         TO RSAL-P-PB-APLICAR
           MOVE AUX-PPORCEN-REAL         TO RSAL-REAJUSTEO
           MOVE AUX-PCALCULOPB-S         TO RSAL-P-REAUSTE-PB

           IF AUX-PPORCEN-REAL    = ZEROES
              MOVE AUX-PCALCULOPB-S      TO RSAL-P-PB-APLICAR
              IF AUX-CCUADROBAREMO-S EQUAL ZEROES
              MOVE SPACES                TO RSAL-BAREMO
              END-IF
              IF AUX-PCALCULOPB-S = ZEROES
                 MOVE ZEROES TO RSAL-P-PB-APLICAR
                 IF AUX-CCUADROBAREMO-S EQUAL ZEROES
                 MOVE SPACES              TO RSAL-BAREMO
                 END-IF
              END-IF
           ELSE
              MOVE AUX-PPORCEN-REAL      TO RSAL-P-PB-APLICAR

           END-IF

           IF AUX-PPORCEN-REAL NOT = ZEROES AND
              AUX-PCALCULOPB-S NOT = ZEROES
              MOVE AUX-CCUADROBAREMO-S TO RSAL-BAREMO
PBTEC         MOVE AUX-PCALCULOPB-S    TO RSAL-P-PB-APLICAR
           END-IF

           MOVE AUX-PGASTOS-S            TO RSAL-P-GASTOS-PB

160824*    IF SW-SI-LTB
160824*       MOVE WS-PRREAPB-BS-ACTUAL  TO RSAL-P-PB-APLICAR
160824*       MOVE WS-PRREAPB-BS-ACTUAL  TO RSAL-P-REAUSTE-PB
160824*       MOVE WS-PRREAPB-BS-ACTUAL  TO AUX-PCALCULOPB-S
160824*       MOVE WS-PGASTOS-BS-ACTUAL  TO RSAL-P-GASTOS-PB
160824*    END-IF.

JOSEN *    COMPUTE RSAL-PRIMA-ANUAL ROUNDED =  WS-IMPORTE-PRI
JOSEN      COMPUTE RSAL-PRIMA-ANUAL ROUNDED =  X350JXX2-EPNETA

JOSE16     MOVE X350JXX2-EPNETA          TO X350JPX6-PRIMA-ANUAL
           MOVE ZEROES                   TO RSAL-GASTOS
                                            X350JPX6-GASTOS
           MOVE X350X098-IARRASTRE-S TO RSAL-IMP-ARRAS-PERD
           MOVE ZEROES                   TO RSAL-NETA-COASEGUR
                                            X350JPX6-NETA-COASEGUR

           IF X350JPX3-KGRUEMP = '000'
              MOVE SPACES           TO X350JPX3-KGRUEMP
           END-IF

           IF X350JPX3-KGRUEMP NOT = SPACES
             MOVE X350JPX4-NASEGURADO    TO RSAL-ASE-TOT-EMPRESA
           END-IF

           IF X350JPX3-KGRUEMP NOT = SPACES
              OPEN INPUT ENTRADA-TASE
              PERFORM 2703-LEER-TOTALES-ASEGURADO
                UNTIL X350JPX3-KGRUEMP EQUAL
                      X350JPX4-KGRUEMP
                CLOSE ENTRADA-TASE

              OPEN INPUT ENTRADA-TASE-CP
                 PERFORM 2703-LEER-TOTALES-ASEGURADO-CP
                   UNTIL X350JPX3-KPOLIZA EQUAL
                         X350JPX5-KPOLIZA AND
                         X350JPX3-KSUBPOL EQUAL
                         X350JPX5-KSUBPOL
                   CLOSE ENTRADA-TASE-CP

              IF X350JPX5-PRIMA-ANUAL <= ZEROES
                 MOVE ZEROES TO WS-IMPORTE-REPARTOS
              ELSE
PBTEC *          COMPUTE WS-IMPORTE-REPARTOS = ((WS-IMPORTE-PERIOD -
PBTEC *                                        WS-IMPORTE-206     -
PBTEC *                                        WS-IMP-GASTOS)   *
PBTEC *                                        X350JPX5-PRIMA-ANUAL) /
PBTEC *                                        X350JPX4-PRIMA-ANUAL
PBTEC            COMPUTE WS-IMPORTE-REPARTOS =
PBTEC                                 ((X350JPX4-PERIOD -
PBTEC                                  X350JPX4-SINIES  -
PBTEC                                  X350JPX4-GASTOS) *
PBTEC                                  X350JPX5-PRIMA-ANUAL) /
PBTEC                                  X350JPX4-PRIMA-ANUAL

              END-IF

               MOVE WS-IMPORTE-REPARTOS    TO X350JPX6-INGRESOS-GASTOS

               IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
                  MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
               END-IF

               COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
                                            = WS-IMPORTE-REPARTOS
      *D*
      *        DISPLAY 'X350JPX4-PERIOD        ' X350JPX4-PERIOD
      *        DISPLAY 'X350JPX4-SINIES        ' X350JPX4-SINIES
      *        DISPLAY 'X350JPX4-GASTOS        ' X350JPX4-GASTOS
      *        DISPLAY 'X350JPX4-KSUBPOL       ' X350JPX4-KSUBPOL
      *        DISPLAY 'X350JPX5-PRIMA-ANUAL   ' X350JPX5-PRIMA-ANUAL
      *        DISPLAY 'X350JPX4-PRIMA-ANUAL   ' X350JPX4-PRIMA-ANUAL
      *        DISPLAY 'WS-IMPORTE-PERIOD      ' WS-IMPORTE-PERIOD
      *        DISPLAY 'WS-IMPORTE-206         ' WS-IMPORTE-206
      *        DISPLAY 'WS-IMP-GASTOS          ' WS-IMP-GASTOS
      *        DISPLAY 'X350JPX5-PRIMA-ANUAL   ' X350JPX5-PRIMA-ANUAL
      *        DISPLAY 'X350JPX4-PRIMA-ANUAL   ' X350JPX4-PRIMA-ANUAL
      *        DISPLAY 'WS-IMPORTE-REPARTOS-1  ' WS-IMPORTE-REPARTOS
      *F*

           ELSE
      *D*
      *        DISPLAY 'ME VOY POR ELSE SI RENO'
      *        DISPLAY 'X350JPX4-KPOLIZA       ' X350JPX4-KPOLIZA
      *        DISPLAY 'X350JPX3-KPOLIZA       ' X350JPX3-KPOLIZA
      *        DISPLAY 'X350JPX3-KSUBPOL       ' X350JPX3-KSUBPOL
      *        DISPLAY 'X350JPX4-KSUBPOL       ' X350JPX4-KSUBPOL
      *        DISPLAY 'X350JPX3-KGRUEMP       ' X350JPX3-KGRUEMP
      *        DISPLAY 'X350JPX4-KGRUEMP       ' X350JPX4-KGRUEMP
      *F*
               OPEN INPUT ENTRADA-TASE-NE
                  PERFORM 2703-LEER-TOTALES-ASEGURADO-NE
                    UNTIL X350JPX3-KPOLIZA EQUAL
                          X350JPX4-KPOLIZA AND
                          X350JPX3-KSUBPOL EQUAL
                          X350JPX4-KSUBPOL AND
                          X350JPX3-KGRUEMP EQUAL
                          X350JPX4-KGRUEMP
                    CLOSE ENTRADA-TASE-NE

               OPEN INPUT ENTRADA-TASE-CP
                  PERFORM 2703-LEER-TOTALES-ASEGURADO-CP
                    UNTIL X350JPX3-KPOLIZA EQUAL
                          X350JPX5-KPOLIZA AND
                          X350JPX3-KSUBPOL EQUAL
                          X350JPX5-KSUBPOL
                    CLOSE ENTRADA-TASE-CP

               IF X350JPX5-PRIMA-ANUAL <= ZEROES
                  MOVE ZEROES TO WS-IMPORTE-REPARTOS
               ELSE
                  COMPUTE WS-IMPORTE-REPARTOS = ((WS-IMPORTE-PERIOD -
MF001 *                                         WS-IMPORTE-206    -
MF001                                           TB-ACUMULADO-206 (1) -
                                                WS-IMP-GASTOS) *
                                                X350JPX5-PRIMA-ANUAL) /
                                                X350JPX5-PRIMA-ANUAL

               END-IF
                   MOVE WS-IMPORTE-REPARTOS TO X350JPX6-INGRESOS-GASTOS

                   IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
                      MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
                   END-IF

                   COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
                                                = WS-IMPORTE-REPARTOS

             END-IF

                COMPUTE WS-NETA-COASEGUR ROUNDED
                                         = (WS-IMPORTE-REPARTOS *
                                            X350JPX3-PCOASE) / 100
                MOVE WS-NETA-COASEGUR         TO X350JPX6-NETA-COASEGUR

                IF X350JPX6-NETA-COASEGUR NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-NETA-COASEGUR
                END-IF

                COMPUTE RSAL-NETA-COASEGUR ROUNDED
                                           = WS-NETA-COASEGUR

                MOVE RSAL-P-PB-APLICAR  TO AUX-PB-TECNICA

                COMPUTE RSAL-PB-TECNICA ROUNDED
                                        = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100
                COMPUTE X350JPX6-PB-TECNICA = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100

                IF X350JPX6-PB-TECNICA NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-PB-TECNICA
                END-IF

                MOVE X350JPX3-PCOASE          TO AUX-PCOASE

                COMPUTE X350JPX6-DEV-NETCOASE = (WS-NETA-COASEGUR *
                                       AUX-PB-TECNICA) / 100

                IF X350JPX6-DEV-NETCOASE < ZEROES
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                   MOVE ZEROES TO RSAL-DEV-NETCOASE
                ELSE
                   COMPUTE RSAL-DEV-NETCOASE ROUNDED
                                          = (WS-NETA-COASEGUR *
                                          AUX-PB-TECNICA) / 100
                END-IF

                IF X350JPX6-DEV-NETCOASE NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                END-IF
      *D*
      *        DISPLAY 'WS-IMPORTE-REPARTOS    ' WS-IMPORTE-REPARTOS
      *        DISPLAY 'WS-NETA-COASEGUR       ' WS-NETA-COASEGUR
      *F*
      *
           IF RSAL-PGLOBAL = '0'
              MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA-REF1
              MOVE '/'                      TO RSAL-SEPARADOR
              MOVE '0000'                   TO RSAL-SUBPOL-REF1
           END-IF

           IF RSAL-POLIZA-REF1 = '0000000'
              MOVE SPACES TO RSAL-POLIZA-REF1
              MOVE SPACES TO RSAL-SEPARADOR
              MOVE SPACES TO RSAL-SUBPOL-REF1
           END-IF

           PERFORM 2200-RECUPERAR-UOA
           PERFORM 2300-OBTENER-NEW-GAP

LUNA       MOVE RSAL-FCIERREA TO WS-FECSIN-ACIERRE
LUNA       MOVE RSAL-FCIERREM TO WS-FECSIN-MCIERRE
LUNA       MOVE RSAL-FCIERRED TO WS-FECSIN-DCIERRE
LUNA
LUNN       IF WS-RENOVADA = 'N' AND WS-ANTPBAN = 'S' AND RSAL-LTA = 'PB'
LUNN          MOVE WS-FECSIN-DESDE-PB TO WS-FECSIN-DESDE
           END-IF

LUNA       MOVE WS-FECSIN-DESDE TO RSAL-FECSIN-DESDE
      *D*
      *    DISPLAY 'WS-FECSIN-DESDE ' WS-FECSIN-DESDE
      *F*
LUNA       IF WS-FECSIN-CIERRE < WS-FECSIN-HASTA
LUNA          MOVE WS-FECSIN-CIERRE TO RSAL-FECSIN-HASTA
LUNA       ELSE
LUNA          MOVE WS-FECSIN-HASTA TO RSAL-FECSIN-HASTA
LUNA       END-IF
           IF RSAL-REAJUSTE = 'S' AND  RSAL-REAJUSTE-PB = SPACES
              MOVE 'POL CON REAJUSTE CON SU TIPO PENDIENTE'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'B350JA12'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE '01'                TO X350J000-ID-ERROR-MOD
              PERFORM 2400-ESCRIBIR-ERRORES
           ELSE
              IF RSAL-REAJUSTE = 'S' AND
                 RSAL-REAJUSTE-PB NOT = SPACES
                 MOVE 'POL CON REAJUSTE CON SU TIPO INCONGRUENTE'
                                           TO X350J000-DESC-ERROR
                  MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                  MOVE 'B350JA12'          TO X350J000-ID-MODULO
                  MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                  MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                  MOVE 'W'                 TO X350J000-NIVEL
                  MOVE '02'                TO X350J000-ID-ERROR-MOD
                  PERFORM 2400-ESCRIBIR-ERRORES
                END-IF
           END-IF

           IF RSAL-REAJUSTE = 'N'           AND
              RSAL-REAJUSTE-PB NOT = SPACES OR
              AUX-PCALCULOPB-S NOT = ZEROES OR
              AUX-PPORCEN-REAL NOT = ZEROES
              MOVE 'POL SIN REAJUSTE PERO CON DATOS DEL MISMO'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'B350JA12'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE '03'                TO X350J000-ID-ERROR-MOD

              PERFORM 2400-ESCRIBIR-ERRORES
           END-IF

      *    MOVE WS-IMPORTE-PBP TO RSAL-PBPAGA
           .
      *                                                                 10890099
BONOA ******************************************************************10650099
       5000-GRABAR-SALIDA-LTB-A.                                        10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5000-GRABAR-SALIDA-LTB-A'
      *    DISPLAY 'WS-IMPORTE-205 ' WS-IMPORTE-205
      *    DISPLAY 'SW-SW-LTPA     ' SW-SW-LTPA
      *F*
           MOVE '3000'                   TO RSAL-MODELO

           MOVE X350JPX3-CCANAL          TO RSAL-CANAL
                                            X350JPX6-CANAL
           MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA
           MOVE X350JPX3-KSUBPOL         TO RSAL-SUBPOLIZA
           MOVE X350JPX3-KSUSCRI         TO RSAL-SUSCRIP

           MOVE X350JPX3-FEFECTO-AINIPOL TO RSAL-FORIGENASUSCRI
           MOVE X350JPX3-FEFECTO-MINIPOL TO RSAL-FORIGENMSUSCRI
           MOVE X350JPX3-FEFECTO-DINIPOL TO RSAL-FORIGENDSUSCRI

           MOVE X350JPX3-KRAMO           TO RSAL-RAMO
                                            X350JPX6-RAMO
           MOVE X350JPX3-MODALI          TO RSAL-MODALIDAD
                                            X350JPX6-MODALIDAD
           MOVE X350JPX3-VIGOR-ANUL      TO RSAL-CESTADOASEG
           MOVE '00'                     TO RSAL-PREGRUPO(1:2)
           MOVE X350JPX3-CARTINV         TO RSAL-PREGRUPO(3:2)
           MOVE X350JPX3-CASADA          TO RSAL-CASADO
           MOVE X350JPX3-PINTER-TECBASE  TO RSAL-I1BTI

           MOVE WS-ANNO-DESDE-TRAMO-ANT  TO RSAL-FRENO-ADESDE
           MOVE WS-MES-DESDE-TRAMO-ANT   TO RSAL-FRENO-MDESDE
           MOVE WS-DIA-DESDE-TRAMO-ANT   TO RSAL-FRENO-DDESDE

           MOVE WS-ANNO-HASTA-TRAMO-ANT  TO RSAL-FRENO-AHASTA
           MOVE WS-MES-HASTA-TRAMO-ANT   TO RSAL-FRENO-MHASTA
           MOVE WS-DIA-HASTA-TRAMO-ANT   TO RSAL-FRENO-DHASTA
           MOVE WS-RENOVADA           TO RSAL-RENOVADA
                                         X350JPX6-RENOVADA

      *    IF WS-FECHA-LTA-H         >  R350T000-FFINAL
      *    OR WS-FECHA-LTA-H         =  R350T000-FFINAL
      *       MOVE 'S'                   TO RSAL-RENOVADA
      *                                     X350JPX6-RENOVADA
      *       MOVE 'S' TO WS-RENOVADA
      *    ELSE
      *       MOVE 'N'                   TO RSAL-RENOVADA
      *                                     X350JPX6-RENOVADA
      *       MOVE 'N' TO WS-RENOVADA
      *    END-IF

JOSEN           IF WS-FECHA-HASTA-TRAMO-ANT < R350T000-FFINAL
JOSEN              MOVE WS-ANNO-HASTA-TRAMO-ANT TO RSAL-FCIERREA
                                                 X350JPX6-FCIERREA
                   MOVE WS-MES-HASTA-TRAMO-ANT  TO RSAL-FCIERREM
                                                 X350JPX6-FCIERREM
JOSEN              MOVE WS-DIA-HASTA-TRAMO-ANT  TO RSAL-FCIERRED
                                                 X350JPX6-FCIERRED
JOSEN           ELSE
                   MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
                                                 X350JPX6-FCIERREA
                   MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
                                                 X350JPX6-FCIERREM
                   MOVE R350T000-FDFINAL         TO RSAL-FCIERRED
                                                 X350JPX6-FCIERRED
JOSEN           END-IF
      *    MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
      *                                     X350JPX6-FCIERREA
      *    MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
      *                                     X350JPX6-FCIERREM
      *    MOVE R350T000-FDFINAL         TO RSAL-FCIERRED
      *                                     X350JPX6-FCIERRED
           MOVE 'BONOA'                  TO RSAL-LTA
           MOVE X350JPX3-FCOBRO          TO RSAL-FORMAPAGO
           MOVE X350JPX3-TIPO-COASE      TO RSAL-COASEGURO
           MOVE X350JPX3-PCOASE          TO RSAL-PCOASEGURO
           MOVE X350JPX3-PAGADOR         TO RSAL-PGLOBAL

           IF X350JXX2-CERTI EQUAL '000000' AND                         000
              X350JXX2-ASEG-MECANIZADOS = 'N'                           000
              MOVE 'S' TO RSAL-IMNOMINADA                               000
           ELSE
              MOVE 'N' TO RSAL-IMNOMINADA                               000
           END-IF

           MOVE X350JPX3-NASEGURADO      TO RSAL-NASEGURADOS

           IF X350JPX3-VIGOR-ANUL  = 'A'
              MOVE X350JPX3-ANO-ANUL     TO RSAL-FECAANULACION
              MOVE X350JPX3-MES-ANUL     TO RSAL-FECMANULACION
              MOVE X350JPX3-DIA-ANUL     TO RSAL-FECDANULACION
              MOVE X350JPX3-MOTIVO-ANU   TO RSAL-MOTIVO
           END-IF

           MOVE 1                        TO RSAL-ALFA

130624     MOVE WS-KGRUPEMP-BS-ANTERIOR TO X350M004-KGRUPEMP-BS
EMPMAR     IF X350M004-KGRUPEMP-BS = ZEROS
EMPMAR        MOVE SPACES                TO X350M004-KGRUPEMP-BS
EMPMAR     END-IF

           MOVE X350M004-KGRUPEMP-BS(3:3) TO RSAL-GEMPRESA

      *    IF AUX-TIP-REAJUSTE = '1' OR 'T'
      *       MOVE 'S'                      TO RSAL-REAJUSTE
      *    ELSE
      *       MOVE 'N'                      TO RSAL-REAJUSTE
      *    END-IF

           MOVE AUX-CCOBON-REAJUS        TO RSAL-REAJUSTE

           MOVE AUX-REAJUSTE-PB          TO RSAL-REAJUSTE-PB

           MOVE WS-REAJREC               TO RSAL-REAJUSTE-REC
           MOVE AUX-TIP-REAJUSTE         TO RSAL-TIP-REAJUSTE
           MOVE AUX-NANIOPER             TO RSAL-APERDIDAS
           MOVE AUX-NANIOBEN             TO RSAL-ABENEFICI

           IF X350X098-IANTICIPO-S > ZEROES
              MOVE 'S'                   TO RSAL-ANTICIPO-PB
           ELSE
              MOVE 'N'                   TO RSAL-ANTICIPO-PB
           END-IF

           MOVE AUX-PANTICIPO-S          TO RSAL-P-ANTICIPO
           MOVE AUX-CCUADROBAREMO-S      TO RSAL-BAREMO
           MOVE AUX-PPORCEN-REAL         TO RSAL-REAJUSTEO
210524*    MOVE X350M004-PPBBONO-BS      TO RSAL-P-PB-APLICAR
210524*    MOVE X350M004-PPBBONO-BS      TO RSAL-P-REAUSTE-PB
210524*    MOVE X350M004-PPBBONO-BS      TO AUX-PCALCULOPB-S
210524*    MOVE X350M004-PGASBONO-BS     TO RSAL-P-GASTOS-PB
210524     MOVE WS-PRREAPB-BS-ANTERIOR   TO RSAL-P-PB-APLICAR
210524     MOVE WS-PRREAPB-BS-ANTERIOR   TO RSAL-P-REAUSTE-PB
210524     MOVE WS-PRREAPB-BS-ANTERIOR   TO AUX-PCALCULOPB-S
210524     MOVE WS-PGASTOS-BS-ANTERIOR   TO RSAL-P-GASTOS-PB

      *D*
      *    DISPLAY 'WS-IMPORTE-205 ' WS-IMPORTE-205
      *    DISPLAY 'WS-IMPORTE-PRI ' WS-IMPORTE-PRI
      *F*
           COMPUTE WS-IMPORTE-LTB = WS-IMPORTE-205 + WS-IMPORTE-PRI

      *D*
      *    DISPLAY 'WS-IMPORTE-LTB ' WS-IMPORTE-LTB
      *F*

      *    COMPUTE RSAL-PRIMA-RECIBOS ROUNDED = WS-IMPORTE-LTB
           MOVE WS-IMPORTE-205           TO RSAL-PRIMA-RECIBOS
                                            X350JPX6-PRIMA-RECIBOS

JOSE  *2301IF SW-SI-LTA-FINAL AND SW-SW-LTPA EQUAL 'N'
JOSE       IF SW-SI-LTA-FINAL AND SW-SW-LTPA EQUAL 'N'
              MOVE WS-IMPORTE-205-BONO-NO-RENO TO RSAL-PRIMA-RECIBOS
                                                  X350JPX6-PRIMA-RECIBOS
           END-IF
           COMPUTE RSAL-APUNTE-RECIBO ROUNDED = WS-IMPORTE-902

           MOVE WS-IMPORTE-902           TO X350JPX6-APUNTE-RECIBO

311224*    IF SW-SI-LTA-FINAL
PLURIN*       COMPUTE WS-IMPORTE-PERIOD-LTA ROUNDED
PLURIN*               = WS-IMPORTE-PERIOD-BONO-ANT
PLURIN*    ELSE
LTA           COMPUTE WS-IMPORTE-PERIOD-LTA ROUNDED
                      = (WS-IMPORTE-205 +
                         WS-IMPORTE-902)
PLURIN*    END-IF

      *D*
      *         DISPLAY ' WS-IMPORTE-PERIOD-LTA '  WS-IMPORTE-PERIOD-LTA
      *F*
100124*2301IF SW-SI-LTA-FINAL AND SW-SW-LTPA EQUAL 'N'
100124     IF SW-SI-LTA-FINAL AND SW-SW-LTPA EQUAL 'N'
100124        COMPUTE WS-IMPORTE-PERIOD-LTA ROUNDED
100124                = WS-IMPORTE-205-BONO-NO-RENO
100124     END-IF

      *D*
      *         DISPLAY ' WS-IMPORTE-PERIOD     '  WS-IMPORTE-PERIOD
      *         DISPLAY ' WS-IMPORTE-PERIOD-LTA '  WS-IMPORTE-PERIOD-LTA
      *         DISPLAY ' X350JPX3-EPNETA       '  X350JPX3-EPNETA
      *         DISPLAY ' 205-NO-RENO ' WS-IMPORTE-205-BONO-NO-RENO
      *         DISPLAY ' WS-IMPORTE-205 ' WS-IMPORTE-205
      *         DISPLAY ' WS-IMPORTE-902 ' WS-IMPORTE-902
      *         DISPLAY ' SW-SW-LTPA     ' SW-SW-LTPA
      *F*
           COMPUTE RSAL-PRIMA-PERIOD ROUNDED = WS-IMPORTE-PERIOD-LTA


           MOVE WS-IMPORTE-PERIOD        TO X350JPX6-PRIMA-PERIOD

           COMPUTE WS-IMP-GASTOS = (WS-IMPORTE-PERIOD *
210524*                     X350M004-PGASBONO-BS) / 100
210524                    WS-PGASTOS-BS-ANTERIOR) / 100

LTA        COMPUTE WS-IMP-GASTOS-LTA = (WS-IMPORTE-PERIOD-LTA *
210524*                     X350M004-PGASBONO-BS) / 100
210524                    WS-PGASTOS-BS-ANTERIOR) / 100

      *D*
      *    DISPLAY 'WS-IMP-GASTOS  ' WS-IMP-GASTOS
      *    DISPLAY 'WS-IMP-GASTOS-LTA ' WS-IMP-GASTOS-LTA
      *F*
           MOVE WS-IMP-GASTOS            TO X350JPX6-GASTOS

           COMPUTE RSAL-GASTOS ROUNDED = WS-IMP-GASTOS-LTA

MF001 *    MOVE WS-IMPORTE-206           TO X350JPX6-TOTAL-SINIES
           MOVE TB-ACUMULADO-206 (6)     TO X350JPX6-TOTAL-SINIES
      *D*
      *    DISPLAY 'WS-IMPORTE-206 ' WS-IMPORTE-206
      *F*

MF001 *    COMPUTE RSAL-TOTAL-SINIES ROUNDED = WS-IMPORTE-206
MF001      COMPUTE RSAL-TOTAL-SINIES ROUNDED = TB-ACUMULADO-206 (6)

           MOVE X350X098-IARRASTRE-S     TO RSAL-IMP-ARRAS-PERD

      *    MOVE X350M004-PGASBONO-BS     TO RSAL-P-GASTOS-PB

      *    COMPUTE RSAL-PRIMA-ANUAL ROUNDED =  WS-IMPORTE-PRI
josen      COMPUTE RSAL-PRIMA-ANUAL ROUNDED = X350JPX3-EPNETA

jose16     MOVE X350JPX3-EPNETA          TO X350JPX6-PRIMA-ANUAL
           MOVE X350X098-IARRASTRE-S TO RSAL-IMP-ARRAS-PERD

           MOVE WS-IMPORTE-PRI           TO X350JPX6-PRIMA-ANUAL
           MOVE X350X098-IARRASTRE-S TO RSAL-IMP-ARRAS-PERD
           MOVE ZEROES                   TO RSAL-NETA-COASEGUR
                                            X350JPX6-NETA-COASEGUR
           MOVE SPACES TO X350JPX5-GEMPRESA

0124       IF SW-SI-LTA-FINAL AND SW-SW-LTPA EQUAL 'N'
                              AND WS-RENOVADA-LTA EQUAL 'N'
                              AND SW-SW-LTP EQUAL 'S'

              COMPUTE RSAL-PRIMA-PERIOD ROUNDED = WS-IMPORTE-205 +
                                                  WS-IMPORTE-205-BONO

              COMPUTE WS-IMPORTE-PERIOD ROUNDED = WS-IMPORTE-205 +
                                                  WS-IMPORTE-205-BONO

              COMPUTE WS-IMPORTE-PERIOD-LTA ROUNDED = WS-IMPORTE-205 +
                                                     WS-IMPORTE-205-BONO

              COMPUTE RSAL-PRIMA-RECIBOS    ROUNDED = WS-IMPORTE-205 +
                                                     WS-IMPORTE-205-BONO

              COMPUTE WS-IMP-GASTOS-LTA = (WS-IMPORTE-PERIOD-LTA *
                                           WS-PGASTOS-BS-ANTERIOR) / 100

              COMPUTE RSAL-GASTOS ROUNDED = WS-IMP-GASTOS-LTA

0124       END-IF
      *D*
      *         DISPLAY ' NUEVO                 '
      *         DISPLAY ' WS-IMPORTE-PERIOD     '  WS-IMPORTE-PERIOD
      *         DISPLAY ' RSAL-PRIMA-RECIBOS    '  RSAL-PRIMA-RECIBOS
      *         DISPLAY ' X350JPX3-EPNETA       '  X350JPX3-EPNETA
      *F*

EMPMAR**   IF X350JPX3-KGRUEMP = '000'
EMPMAR     IF X350M004-KGRUPEMP-BS  = SPACES
              MOVE SPACES           TO X350JPX3-KGRUEMP
           END-IF

EMPMAR**   IF X350JPX3-KGRUEMP NOT = SPACES
EMPMAR     IF X350M004-KGRUPEMP-BS  NOT = SPACES
             MOVE X350JPX4-NASEGURADO    TO RSAL-ASE-TOT-EMPRESA
           END-IF

           INITIALIZE X350JPX5

      *D*
      *        DISPLAY 'X350JPX3-KGRUEMP         ' X350JPX3-KGRUEMP
      *F*
EMPMAR**   IF X350JPX3-KGRUEMP NOT = SPACES
EMPMAR     IF X350M004-KGRUPEMP-BS NOT = SPACES
              OPEN INPUT ENTRADA-TASE-ANT
              PERFORM 2703-LEER-TOTALES-ANTERIOR
EMPMAR**        UNTIL X350JPX3-KGRUEMP EQUAL
EMPMAR          UNTIL X350M004-KGRUPEMP-BS(3:3) EQUAL
                      X350JPX5-GEMPRESA  AND
                      X350JPX5-LTA = 'BONOA'
                CLOSE ENTRADA-TASE-ANT

      *D*
      *        DISPLAY 'X350JPX5-GEMPRESA         ' X350JPX5-GEMPRESA
      *        DISPLAY 'X350JPX5-PRIMA-PERIOD ANT '
      *                 X350JPX5-PRIMA-PERIOD
      *F*
              MOVE X350JPX5-PBPAGADA-EMP    TO RSAL-PBPAGA-TOT-EMPR
              MOVE X350JPX5-PRIMA-PERIOD    TO RSAL-PRI-TOT-EMPRESA

              IF X350JPX5-TOTAL-SINIES = ZEROES
                 MOVE AUX-SINIESTROS-LTP    TO RSAL-SIN-TOT-EMPRESA
              ELSE
                 MOVE X350JPX5-TOTAL-SINIES TO RSAL-SIN-TOT-EMPRESA
              END-IF

              COMPUTE RSAL-GASTOS-TOT-EMPR = (X350JPX5-PRIMA-PERIOD *
210524*                     X350M004-PGASBONO-BS) / 100
210524                    WS-PGASTOS-BS-ANTERIOR) / 100

081225        MOVE RSAL-GASTOS-TOT-EMPR TO X350JPX5-GASTOS

              MOVE X350JPX5-PBPAGADA-EMP TO RSAL-PBPAGA-TOT-EMPR

      *D*
      *       DISPLAY 'EMPRESA-BONOA         '
      *       DISPLAY 'X350JPX5-PRIMA-PERIOD ' X350JPX5-PRIMA-PERIOD
      *       DISPLAY 'WS-IMPORTE-206        ' X350JPX5-TOTAL-SINIES
      *       DISPLAY 'AUX-GASTOS-LTA        ' AUX-GASTOS-LTA
      *       DISPLAY 'WS-IMPORTE-PRI        ' WS-IMPORTE-PRI
      *       DISPLAY 'X350JPX5-GASTOS       ' X350JPX5-GASTOS
      *       DISPLAY 'X350JPX5-PRIMA-ANUAL  ' X350JPX5-PRIMA-ANUAL
      *       DISPLAY 'X350JPX5-PBPAGADA-EMP ' X350JPX5-PBPAGADA-EMP
      *F*
              COMPUTE WS-IMPORTE-REPARTOS = ((X350JPX5-PRIMA-PERIOD -
PBTEC *                                       WS-IMPORTE-206    -
PBTEC                                         X350JPX5-TOTAL-SINIES -
                                              X350JPX5-GASTOS   -
                                              X350JPX5-PBPAGADA-EMP) *
                                              X350JPX3-EPNETA) /
                                              X350JPX5-PRIMA-ANUAL
PLURIN     ELSE
PLURIN
PLURIN        OPEN INPUT ENTRADA-TASE-ANT
PLURIN        PERFORM 2703-LEER-TOTALES-ANTERIOR
PLURIN          UNTIL X350JPX3-KPOLIZA EQUAL
PLURIN                X350JPX5-KPOLIZA AND
PLURIN                X350JPX3-KSUBPOL EQUAL
PLURIN                X350JPX5-KSUBPOL AND
                      X350M004-KGRUPEMP-BS(3:3) EQUAL
                      X350JPX5-GEMPRESA
              CLOSE ENTRADA-TASE-ANT

      *D*
      *       DISPLAY 'NO-EMPRESA-BONOA         '
      *       DISPLAY 'X350JPX5-PRIMA-PERIOD ' X350JPX5-PRIMA-PERIOD
      *       DISPLAY 'WS-IMPORTE-206        ' X350JPX5-TOTAL-SINIES
      *       DISPLAY 'AUX-GASTOS-LTA        ' AUX-GASTOS-LTA
      *       DISPLAY 'WS-IMPORTE-PRI        ' WS-IMPORTE-PRI
      *       DISPLAY 'X350JPX5-GASTOS       ' X350JPX5-GASTOS
      *       DISPLAY 'X350JPX5-PRIMA-ANUAL  ' X350JPX5-PRIMA-ANUAL
      *       DISPLAY 'X350JPX5-PBPAGADA-EMP ' X350JPX5-PBPAGADA-EMP
      *F*
PLURIN        COMPUTE WS-IMPORTE-REPARTOS = ((X350JPX5-PRIMA-PERIOD -
PLURIN                                        X350JPX5-TOTAL-SINIES -
RF001****                                     X350JPX5-GASTOS   -
RF001                                         WS-IMP-GASTOS-LTA -
PLURIN                                        X350JPX5-PBPAGADA-EMP) *
PLURIN                                        X350JPX3-EPNETA) /
PLURIN                                        X350JPX5-PRIMA-ANUAL
           END-IF
      *D*
      *       DISPLAY 'X350JXX2-KPOLIZA      ' X350JXX2-KPOLIZA
      *       DISPLAY 'X350JXX2-KSUBPOL      ' X350JXX2-KSUBPOL
      *       DISPLAY 'X350JPX3-KPOLIZA      ' X350JPX3-KPOLIZA
      *       DISPLAY 'X350JPX3-KSUBPOL      ' X350JPX3-KSUBPOL
      *       DISPLAY 'X350JPX5-PRIMA-PERIOD ' X350JPX5-PRIMA-PERIOD
      *       DISPLAY 'AUX-GASTOS-LTA        ' AUX-GASTOS-LTA
      *       DISPLAY 'X350JPX5-PBPAGADA-EMP ' X350JPX5-PBPAGADA-EMP
      *       DISPLAY 'X350JPX5-GASTOS       ' X350JPX5-GASTOS
      *       DISPLAY 'X350JPX5-PRIMA-ANUAL  ' X350JPX5-PRIMA-ANUAL
      *       DISPLAY 'X350JPX5-TOTAL-SINIES ' X350JPX5-TOTAL-SINIES
      *       DISPLAY 'X350M004-KGRUPEMP-BS ' X350M004-KGRUPEMP-BS(3:3)
      *F*
              MOVE WS-IMPORTE-REPARTOS TO X350JPX6-INGRESOS-GASTOS

              IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
                 MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
              END-IF

               COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
                                         = WS-IMPORTE-REPARTOS

               COMPUTE WS-NETA-COASEGUR ROUNDED
                                         = (WS-IMPORTE-REPARTOS *
                                            X350JPX3-PCOASE) / 100
                MOVE WS-NETA-COASEGUR         TO X350JPX6-NETA-COASEGUR

                IF X350JPX6-NETA-COASEGUR NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-NETA-COASEGUR
                END-IF

                COMPUTE RSAL-NETA-COASEGUR ROUNDED
                                           = WS-NETA-COASEGUR

                MOVE RSAL-P-PB-APLICAR  TO AUX-PB-TECNICA

                COMPUTE RSAL-PB-TECNICA ROUNDED
                                        = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100
                COMPUTE X350JPX6-PB-TECNICA = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100

                IF X350JPX6-PB-TECNICA NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-PB-TECNICA
                END-IF

                MOVE X350JPX3-PCOASE          TO AUX-PCOASE

                COMPUTE X350JPX6-DEV-NETCOASE = (WS-NETA-COASEGUR *
210524*                                X350M004-PPBBONO-BS) / 100
210524                             WS-PRREAPB-BS-ANTERIOR) / 100

      *D*
      *         DISPLAY 'WS-NETA-COASEGUR ' WS-NETA-COASEGUR
      *         DISPLAY 'X350M004-PPBBONO-BS ' X350M004-PPBBONO-BS
      *F*
                IF X350JPX6-DEV-NETCOASE < ZEROES
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                   MOVE ZEROES TO RSAL-DEV-NETCOASE
                ELSE
                   COMPUTE RSAL-DEV-NETCOASE ROUNDED
                                          = (WS-NETA-COASEGUR *
210524*                                X350M004-PPBBONO-BS) / 100
210524                             WS-PRREAPB-BS-ANTERIOR) / 100
      *D*
      *         DISPLAY 'RSAL-DEV-NETCOASE ' RSAL-DEV-NETCOASE
      *F*
                END-IF

                IF X350JPX6-DEV-NETCOASE NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                END-IF

PLURIN*    END-IF

           IF RSAL-PGLOBAL = '0'
              MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA-REF1
              MOVE '/'                      TO RSAL-SEPARADOR
              MOVE '0000'                   TO RSAL-SUBPOL-REF1
           END-IF

           IF RSAL-POLIZA-REF1 = '0000000'
              MOVE SPACES TO RSAL-POLIZA-REF1
              MOVE SPACES TO RSAL-SEPARADOR
              MOVE SPACES TO RSAL-SUBPOL-REF1
           END-IF



           PERFORM 2210-RECUPERAR-UOA-NR
           PERFORM 2300-OBTENER-NEW-GAP

           IF RSAL-REAJUSTE = 'S' AND  RSAL-REAJUSTE-PB = SPACES
              MOVE 'POL CON REAJUSTE CON SU TIPO PENDIENTE'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JX99'          TO X350J000-ID-PROGRAMA
              MOVE 'B350JX99'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE '01'                TO X350J000-ID-ERROR-MOD
              PERFORM 2400-ESCRIBIR-ERRORES
           ELSE
              IF RSAL-REAJUSTE = 'S' AND
                 RSAL-REAJUSTE-PB NOT = SPACES
                 MOVE 'POL CON REAJUSTE CON SU TIPO INCONGRUENTE'
                                           TO X350J000-DESC-ERROR
                  MOVE 'B350JX99'          TO X350J000-ID-PROGRAMA
                  MOVE 'B350JX99'          TO X350J000-ID-MODULO
                  MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                  MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                  MOVE 'W'                 TO X350J000-NIVEL
                  MOVE '02'                TO X350J000-ID-ERROR-MOD
                  PERFORM 2400-ESCRIBIR-ERRORES
                END-IF
           END-IF

           IF RSAL-REAJUSTE = 'N'           AND
              RSAL-REAJUSTE-PB NOT = SPACES OR
              AUX-PCALCULOPB-S NOT = ZEROES OR
              AUX-PPORCEN-REAL NOT = ZEROES
              MOVE 'POL SIN REAJUSTE PERO CON DATOS DEL MISMO'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JX99'          TO X350J000-ID-PROGRAMA
              MOVE 'B350JX99'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE '03'                TO X350J000-ID-ERROR-MOD

              PERFORM 2400-ESCRIBIR-ERRORES
           END-IF

      *    PERFORM 5214-CALCULOS-LTB
170624*    MOVE WS-IMPORTE-PBP-ANT     TO RSAL-PBPAGA
170624        COMPUTE RSAL-PBPAGA = WS-IMPORTE-PBP-ANT +
                                 WS-IMPORTE-PBP-BONO-FASE2
      *D*
      *         DISPLAY 'PBP-ANT-BONO-ANT' WS-IMPORTE-PBP-ANT
      *         DISPLAY 'RSAL-PBPAGA     ' RSAL-PBPAGA
      *F*
LUNA       MOVE RSAL-FCIERREA TO WS-FECSIN-ACIERRE
LUNA       MOVE RSAL-FCIERREM TO WS-FECSIN-MCIERRE
LUNA       MOVE RSAL-FCIERRED TO WS-FECSIN-DCIERRE
LUNA
LUNA       MOVE WS-FECSIN-DESDE TO RSAL-FECSIN-DESDE
LUNA       IF WS-FECSIN-CIERRE < WS-FECSIN-HASTA
LUNA          MOVE WS-FECSIN-CIERRE TO RSAL-FECSIN-HASTA
LUNA       ELSE
LUNA          MOVE WS-FECSIN-HASTA TO RSAL-FECSIN-HASTA
LUNA       END-IF

           WRITE REG-SALIDA1 FROM RSALIDA

           MOVE ZEROES TO WS-IMPORTE-PERIOD-LTA

      *    MOVE WS-POLIZA-2 TO WS-POLIZA-2-ANT
      *    MOVE WS-SUBPOL-2 TO WS-SUBPOL-2-ANT
      *    MOVE WS-POLIZA-4 TO WS-POLIZA-4-ANT
      *    MOVE WS-SUBPOL-4 TO WS-SUBPOL-4-ANT

           INITIALIZE RSALIDA
           SET SW-NO-LTBA TO TRUE
0124       SET SW-NO-LTPA TO TRUE
LTA        .

PLURA ******************************************************************10650099
       5000-GRABAR-SALIDA-LTP-A.                                        10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5000-GRABAR-SALIDA-LTP-A'
      *    DISPLAY 'X350JPX3-KPOLIZA        ' X350JPX3-KPOLIZA
      *    DISPLAY 'X350JPX3-KSUBPOL        ' X350JPX3-KSUBPOL
      *    DISPLAY 'SW-SW-LTPA              ' SW-SW-LTPA
      *F*
           MOVE '3000'                   TO RSAL-MODELO

           MOVE X350JPX3-CCANAL          TO RSAL-CANAL
                                            X350JPX6-CANAL
           MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA
           MOVE X350JPX3-KSUBPOL         TO RSAL-SUBPOLIZA
           MOVE X350JPX3-KSUSCRI         TO RSAL-SUSCRIP

           MOVE X350JPX3-FEFECTO-AINIPOL TO RSAL-FORIGENASUSCRI
           MOVE X350JPX3-FEFECTO-MINIPOL TO RSAL-FORIGENMSUSCRI
           MOVE X350JPX3-FEFECTO-DINIPOL TO RSAL-FORIGENDSUSCRI

           MOVE X350JPX3-KRAMO           TO RSAL-RAMO
                                            X350JPX6-RAMO
           MOVE X350JPX3-MODALI          TO RSAL-MODALIDAD
                                            X350JPX6-MODALIDAD
           MOVE X350JPX3-VIGOR-ANUL      TO RSAL-CESTADOASEG
           MOVE '00'                     TO RSAL-PREGRUPO(1:2)
           MOVE X350JPX3-CARTINV         TO RSAL-PREGRUPO(3:2)
           MOVE X350JPX3-CASADA          TO RSAL-CASADO
           MOVE X350JPX3-PINTER-TECBASE  TO RSAL-I1BTI

           MOVE WS-ANNO-DESDE-TRAMO-ANT  TO RSAL-FRENO-ADESDE
           MOVE WS-MES-DESDE-TRAMO-ANT   TO RSAL-FRENO-MDESDE
           MOVE WS-DIA-DESDE-TRAMO-ANT   TO RSAL-FRENO-DDESDE

           MOVE WS-ANNO-HASTA-TRAMO-ANT  TO RSAL-FRENO-AHASTA
           MOVE WS-MES-HASTA-TRAMO-ANT   TO RSAL-FRENO-MHASTA
           MOVE WS-DIA-HASTA-TRAMO-ANT   TO RSAL-FRENO-DHASTA
           MOVE WS-RENOVADA              TO RSAL-RENOVADA
                                            X350JPX6-RENOVADA
JOSEN           IF WS-FECHA-HASTA-TRAMO-ANT < R350T000-FFINAL
JOSEN              MOVE WS-ANNO-HASTA-TRAMO-ANT TO RSAL-FCIERREA
                                                 X350JPX6-FCIERREA
                   MOVE WS-MES-HASTA-TRAMO-ANT  TO RSAL-FCIERREM
                                                 X350JPX6-FCIERREM
JOSEN              MOVE WS-DIA-HASTA-TRAMO-ANT  TO RSAL-FCIERRED
                                                 X350JPX6-FCIERRED
JOSEN           ELSE
                   MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
                                                 X350JPX6-FCIERREA
                   MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
                                                 X350JPX6-FCIERREM
                   MOVE R350T000-FDFINAL         TO RSAL-FCIERRED
                                                 X350JPX6-FCIERRED
JOSEN           END-IF
      *    MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
      *                                     X350JPX6-FCIERREA
      *    MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
      *                                     X350JPX6-FCIERREM
      *    MOVE R350T000-FDFINAL         TO RSAL-FCIERRED
      *                                     X350JPX6-FCIERRED
           MOVE 'PLURA'                  TO RSAL-LTA
           MOVE X350JPX3-FCOBRO          TO RSAL-FORMAPAGO
           MOVE X350JPX3-TIPO-COASE      TO RSAL-COASEGURO
           MOVE X350JPX3-PCOASE          TO RSAL-PCOASEGURO
           MOVE X350JPX3-PAGADOR         TO RSAL-PGLOBAL

           IF X350JXX2-CERTI EQUAL '000000' AND                         000
              X350JXX2-ASEG-MECANIZADOS = 'N'                           000
              MOVE 'S' TO RSAL-IMNOMINADA                               000
           ELSE
              MOVE 'N' TO RSAL-IMNOMINADA                               000
           END-IF

           MOVE X350JPX3-NASEGURADO      TO RSAL-NASEGURADOS

           IF X350JPX3-VIGOR-ANUL  = 'A'
              MOVE X350JPX3-ANO-ANUL     TO RSAL-FECAANULACION
              MOVE X350JPX3-MES-ANUL     TO RSAL-FECMANULACION
              MOVE X350JPX3-DIA-ANUL     TO RSAL-FECDANULACION
              MOVE X350JPX3-MOTIVO-ANU   TO RSAL-MOTIVO
           END-IF

           MOVE 1                        TO RSAL-ALFA
240524                                      WS-ALFA

130624     MOVE WS-KGRUPEMP-PS-ANTERIOR  TO X350M004-KGRUPEMP-PS
EMPMAR     IF X350M004-KGRUPEMP-PS(3:3) = ZEROS
EMPMAR        MOVE SPACES                TO X350M004-KGRUPEMP-PS
EMPMAR     END-IF

           MOVE X350M004-KGRUPEMP-PS(3:3) TO RSAL-GEMPRESA

           MOVE AUX-CCOBON-REAJUS        TO RSAL-REAJUSTE

           MOVE AUX-REAJUSTE-PB          TO RSAL-REAJUSTE-PB

           MOVE WS-REAJREC               TO RSAL-REAJUSTE-REC
           MOVE AUX-TIP-REAJUSTE         TO RSAL-TIP-REAJUSTE
           MOVE AUX-NANIOPER             TO RSAL-APERDIDAS
           MOVE AUX-NANIOBEN             TO RSAL-ABENEFICI

           IF X350X098-IANTICIPO-S > ZEROES
              MOVE 'S'                   TO RSAL-ANTICIPO-PB
           ELSE
              MOVE 'N'                   TO RSAL-ANTICIPO-PB
           END-IF

           MOVE AUX-PANTICIPO-S          TO RSAL-P-ANTICIPO
           MOVE AUX-CCUADROBAREMO-S      TO RSAL-BAREMO
           MOVE AUX-PPORCEN-REAL         TO RSAL-REAJUSTEO
      *    MOVE X350M004-PREAJPB-PS      TO RSAL-P-PB-APLICAR
210524*    MOVE X350M004-PREAJPB-PS      TO RSAL-P-REAUSTE-PB
210524*    MOVE X350M004-PREAJPB-PS      TO AUX-PCALCULOPB-S
210524*    MOVE X350M004-PGASTOS-PS      TO RSAL-P-GASTOS-PB
210524     MOVE WS-PRREAPB-PS-ANTERIOR   TO RSAL-P-PB-APLICAR
210524     MOVE WS-PRREAPB-PS-ANTERIOR   TO RSAL-P-REAUSTE-PB
210524     MOVE WS-PRREAPB-PS-ANTERIOR   TO AUX-PCALCULOPB-S
210524     MOVE WS-PGASTOS-PS-ANTERIOR   TO RSAL-P-GASTOS-PB

      *D*
      *    DISPLAY 'WS-PRREAPB-PS-ANTERIOR ' WS-PRREAPB-PS-ANTERIOR
      *    DISPLAY 'WS-PGASTOS-PS-ANTERIOR ' WS-PGASTOS-PS-ANTERIOR
      *F*
           COMPUTE WS-IMPORTE-LTB = WS-IMPORTE-205 + WS-IMPORTE-PRI


      *D*
      *    DISPLAY 'WS-IMPORTE-LTB ' WS-IMPORTE-LTB
      *    DISPLAY 'WS-IMPORTE-205 ' WS-IMPORTE-205
      *F*

           MOVE WS-IMPORTE-205           TO RSAL-PRIMA-RECIBOS
                                            X350JPX6-PRIMA-RECIBOS
      *D*
      *    DISPLAY 'RSAL-PRIMA-RECIBOS ' RSAL-PRIMA-RECIBOS
      *F*
           COMPUTE RSAL-APUNTE-RECIBO ROUNDED = WS-IMPORTE-902

           MOVE WS-IMPORTE-902           TO X350JPX6-APUNTE-RECIBO

311224*    IF SW-SI-LTA-FINAL
PLURIN*       COMPUTE WS-IMPORTE-PERIOD-LTA ROUNDED
PLURIN*               = WS-IMPORTE-PERIOD-BONO-ANT
PLURIN*    ELSE
LTA           COMPUTE WS-IMPORTE-PERIOD-LTA ROUNDED
                      = (WS-IMPORTE-205 +
                         WS-IMPORTE-902)
PLURIN*    END-IF

      *D*
      *         DISPLAY ' WS-IMPORTE-PERIOD-LTA '  WS-IMPORTE-PERIOD-LTA
      *         DISPLAY ' PERIOD-BONO-ANT ' WS-IMPORTE-PERIOD-BONO-ANT
      *F*
           COMPUTE WS-IMPORTE-PERIOD ROUNDED
                   = (WS-IMPORTE-LTB +
                      WS-IMPORTE-902)
      *D*
      *         DISPLAY ' WS-IMPORTE-PERIOD     '  WS-IMPORTE-PERIOD
      *F*

           COMPUTE RSAL-PRIMA-PERIOD ROUNDED = WS-IMPORTE-PERIOD-LTA


           MOVE WS-IMPORTE-PERIOD        TO X350JPX6-PRIMA-PERIOD

           COMPUTE WS-IMP-GASTOS = (WS-IMPORTE-PERIOD *
210524*                     X350M004-PGASTOS-PS) / 100
210524                      WS-PGASTOS-PS-ANTERIOR) / 100

LTA        COMPUTE WS-IMP-GASTOS-LTA = (WS-IMPORTE-PERIOD-LTA *
210524*                     X350M004-PGASTOS-PS) / 100
210524                      WS-PGASTOS-PS-ANTERIOR) / 100

      *D*
      *    DISPLAY 'WS-IMP-GASTOS  ' WS-IMP-GASTOS
      *    DISPLAY 'WS-IMP-GASTOS-LTA ' WS-IMP-GASTOS-LTA
      *    DISPLAY 'X350M004-PGASTOS-PS ' X350M004-PGASTOS-PS
      *F*
           MOVE WS-IMP-GASTOS            TO X350JPX6-GASTOS

           COMPUTE RSAL-GASTOS ROUNDED = WS-IMP-GASTOS-LTA

PLURIN     IF SW-NO-LTP AND SW-SI-LTA-FINAL
PLURIN        COMPUTE WS-IMP-GASTOS = (WS-IMPORTE-PERIOD-BONO-ANT *
PLURIN                WS-PGASTOS-BS-ANTERIOR) / 100
PLURIN        COMPUTE RSAL-GASTOS = WS-IMP-GASTOS
PLURIN     END-IF

MF001 *    MOVE WS-IMPORTE-206           TO X350JPX6-TOTAL-SINIES
MF001      MOVE TB-ACUMULADO-206 (4)     TO X350JPX6-TOTAL-SINIES

MF001 *    COMPUTE RSAL-TOTAL-SINIES ROUNDED = WS-IMPORTE-206
MF001      COMPUTE RSAL-TOTAL-SINIES ROUNDED = TB-ACUMULADO-206 (4)

           MOVE X350X098-IARRASTRE-S     TO RSAL-IMP-ARRAS-PERD

210524*    MOVE X350M004-PGASTOS-PS      TO RSAL-P-GASTOS-PB

JOSE16*    COMPUTE RSAL-PRIMA-ANUAL ROUNDED =  WS-IMPORTE-PRI
josen *    COMPUTE RSAL-PRIMA-ANUAL ROUNDED = X350JXX2-EPNETA
josen      COMPUTE RSAL-PRIMA-ANUAL ROUNDED = X350JPX3-EPNETA

JOSE16     MOVE X350JPX3-EPNETA          TO X350JPX6-PRIMA-ANUAL
           MOVE X350X098-IARRASTRE-S TO RSAL-IMP-ARRAS-PERD

           MOVE X350JPX3-EPNETA          TO X350JPX6-PRIMA-ANUAL
           MOVE X350X098-IARRASTRE-S TO RSAL-IMP-ARRAS-PERD
           MOVE ZEROES                   TO RSAL-NETA-COASEGUR
                                            X350JPX6-NETA-COASEGUR

EMPMAR**   IF X350JPX3-KGRUEMP = '000'
EMPMAR     IF X350M004-KGRUPEMP-PS  = SPACES
              MOVE SPACES           TO X350JPX3-KGRUEMP
           END-IF

EMPMAR**   IF X350JPX3-KGRUEMP NOT = SPACES
EMPMAR     IF X350M004-KGRUPEMP-PS   NOT = SPACES
             MOVE X350JPX4-NASEGURADO    TO RSAL-ASE-TOT-EMPRESA
           END-IF

           INITIALIZE X350JPX5

      *D*
      *        DISPLAY 'X350JPX3-KGRUEMP         ' X350JPX3-KGRUEMP
      *F*
EMPMAR**   IF X350JPX3-KGRUEMP NOT = SPACES
EMPMAR     IF X350M004-KGRUPEMP-PS NOT = SPACES
              OPEN INPUT ENTRADA-TASE-ANT
              PERFORM 2703-LEER-TOTALES-ANTERIOR
EMPMAR*         UNTIL X350JPX3-KGRUEMP EQUAL
EMPMAR          UNTIL X350M004-KGRUPEMP-PS(3:3) EQUAL
                      X350JPX5-GEMPRESA  AND
                      X350JPX5-LTA = 'PLURA'
                CLOSE ENTRADA-TASE-ANT

      *D*
      *        DISPLAY 'X350JPX5-KGRUEMP         ' X350JPX5-GEMPRESA
      *        DISPLAY 'X350JPX5-PRIMA-PERIOD ANT '
      *                 X350JPX5-PRIMA-PERIOD
      *F*
              MOVE X350JPX5-PBPAGADA-EMP    TO RSAL-PBPAGA-TOT-EMPR
              MOVE X350JPX5-PRIMA-PERIOD    TO RSAL-PRI-TOT-EMPRESA

              IF X350JPX5-TOTAL-SINIES = ZEROES
                 MOVE AUX-SINIESTROS-LTP    TO RSAL-SIN-TOT-EMPRESA
              ELSE
                 MOVE X350JPX5-TOTAL-SINIES TO RSAL-SIN-TOT-EMPRESA
              END-IF

              COMPUTE RSAL-GASTOS-TOT-EMPR = (X350JPX5-PRIMA-PERIOD *
210524*                     X350M004-PGASTOS-PS) / 100
210524                      WS-PGASTOS-PS-ANTERIOR) / 100

              MOVE X350JPX5-PBPAGADA-EMP TO RSAL-PBPAGA-TOT-EMPR
      *D*
      *       DISPLAY 'EMPRESA-PLURA         '
      *       DISPLAY 'X350JPX5-PRIMA-PERIOD ' X350JPX5-PRIMA-PERIOD
      *       DISPLAY 'WS-IMPORTE-206        ' X350JPX5-TOTAL-SINIES
      *       DISPLAY 'AUX-GASTOS-LTA        ' AUX-GASTOS-LTA
      *       DISPLAY 'WS-IMPORTE-PRI        ' WS-IMPORTE-PRI
      *       DISPLAY 'X350JPX5-GASTOS       ' X350JPX5-GASTOS
      *       DISPLAY 'X350JPX5-PRIMA-ANUAL  ' X350JPX5-PRIMA-ANUAL
      *       DISPLAY 'X350JPX5-PBPAGADA-EMP ' X350JPX5-PBPAGADA-EMP
      *F*

              COMPUTE WS-IMPORTE-REPARTOS = ((X350JPX5-PRIMA-PERIOD -
                                              X350JPX5-TOTAL-SINIES -
                                              X350JPX5-GASTOS   -
                                              X350JPX5-PBPAGADA-EMP) *
                                              X350JPX3-EPNETA) /
                                              X350JPX5-PRIMA-ANUAL

PLURIN     ELSE
PLURIN
PLURIN        OPEN INPUT ENTRADA-TASE-ANT
PLURIN        PERFORM 2703-LEER-TOTALES-ANTERIOR
PLURIN          UNTIL X350JPX3-KPOLIZA EQUAL
PLURIN                X350JPX5-KPOLIZA AND
PLURIN                X350JPX3-KSUBPOL EQUAL
PLURIN                X350JPX5-KSUBPOL AND
                      X350M004-KGRUPEMP-PS(3:3) EQUAL
                      X350JPX5-GEMPRESA
              CLOSE ENTRADA-TASE-ANT

      *D*
      *       DISPLAY 'NO-EMPRESA-PLURA         '
      *       DISPLAY 'X350JPX5-PRIMA-PERIOD ' X350JPX5-PRIMA-PERIOD
      *       DISPLAY 'WS-IMPORTE-206        ' X350JPX5-TOTAL-SINIES
      *       DISPLAY 'AUX-GASTOS-LTA        ' AUX-GASTOS-LTA
      *       DISPLAY 'WS-IMPORTE-PRI        ' WS-IMPORTE-PRI
      *       DISPLAY 'X350JPX5-GASTOS       ' X350JPX5-GASTOS
      *       DISPLAY 'X350JPX5-PRIMA-ANUAL  ' X350JPX5-PRIMA-ANUAL
      *       DISPLAY 'X350JPX5-PBPAGADA-EMP ' X350JPX5-PBPAGADA-EMP
      *F*
PLURIN        COMPUTE WS-IMPORTE-REPARTOS = ((X350JPX5-PRIMA-PERIOD -
PLURIN                                        X350JPX5-TOTAL-SINIES -
PLURIN                                        X350JPX5-GASTOS   -
PLURIN                                        X350JPX5-PBPAGADA-EMP) *
PLURIN                                        X350JPX3-EPNETA) /
PLURIN                                        X350JPX5-PRIMA-ANUAL
           END-IF

              MOVE WS-IMPORTE-REPARTOS TO X350JPX6-INGRESOS-GASTOS

              IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
                 MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
              END-IF

               COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
                                         = WS-IMPORTE-REPARTOS

               COMPUTE WS-NETA-COASEGUR ROUNDED
                                         = (WS-IMPORTE-REPARTOS *
                                            X350JPX3-PCOASE) / 100
                MOVE WS-NETA-COASEGUR         TO X350JPX6-NETA-COASEGUR

                IF X350JPX6-NETA-COASEGUR NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-NETA-COASEGUR
                END-IF

                COMPUTE RSAL-NETA-COASEGUR ROUNDED
                                           = WS-NETA-COASEGUR

                MOVE RSAL-P-PB-APLICAR  TO AUX-PB-TECNICA

                COMPUTE RSAL-PB-TECNICA ROUNDED
                                        = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100
                COMPUTE X350JPX6-PB-TECNICA = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100

                IF X350JPX6-PB-TECNICA NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-PB-TECNICA
                END-IF

                MOVE X350JPX3-PCOASE          TO AUX-PCOASE

                COMPUTE X350JPX6-DEV-NETCOASE = (WS-NETA-COASEGUR *
210524*                                X350M004-PREAJPB-PS) / 100
210524                              WS-PRREAPB-PS-ANTERIOR) / 100

      *D*
      *         DISPLAY 'WS-NETA-COASEGUR ' WS-NETA-COASEGUR
      *         DISPLAY 'X350M004-PREAJPB-PS ' X350M004-PREAJPB-PS
      *F*
                IF X350JPX6-DEV-NETCOASE < ZEROES
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                   MOVE ZEROES TO RSAL-DEV-NETCOASE
                ELSE
                   COMPUTE RSAL-DEV-NETCOASE ROUNDED
                                          = (WS-NETA-COASEGUR *
210524*                                X350M004-PREAJPB-PS) / 100
210524                              WS-PRREAPB-PS-ANTERIOR) / 100
      *D*
      *         DISPLAY 'RSAL-DEV-NETCOASE ' RSAL-DEV-NETCOASE
      *F*
                END-IF

                IF X350JPX6-DEV-NETCOASE NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                END-IF

********   END-IF

           IF RSAL-PGLOBAL = '0'
              MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA-REF1
              MOVE '/'                      TO RSAL-SEPARADOR
              MOVE '0000'                   TO RSAL-SUBPOL-REF1
           END-IF

           IF RSAL-POLIZA-REF1 = '0000000'
              MOVE SPACES TO RSAL-POLIZA-REF1
              MOVE SPACES TO RSAL-SEPARADOR
              MOVE SPACES TO RSAL-SUBPOL-REF1
           END-IF



           PERFORM 2210-RECUPERAR-UOA-NR
           PERFORM 2300-OBTENER-NEW-GAP

           IF RSAL-REAJUSTE = 'S' AND  RSAL-REAJUSTE-PB = SPACES
              MOVE 'POL CON REAJUSTE CON SU TIPO PENDIENTE'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JX99'          TO X350J000-ID-PROGRAMA
              MOVE 'B350JX99'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE '01'                TO X350J000-ID-ERROR-MOD
              PERFORM 2400-ESCRIBIR-ERRORES
           ELSE
              IF RSAL-REAJUSTE = 'S' AND
                 RSAL-REAJUSTE-PB NOT = SPACES
                 MOVE 'POL CON REAJUSTE CON SU TIPO INCONGRUENTE'
                                           TO X350J000-DESC-ERROR
                  MOVE 'B350JX99'          TO X350J000-ID-PROGRAMA
                  MOVE 'B350JX99'          TO X350J000-ID-MODULO
                  MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                  MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                  MOVE 'W'                 TO X350J000-NIVEL
                  MOVE '02'                TO X350J000-ID-ERROR-MOD
                  PERFORM 2400-ESCRIBIR-ERRORES
                END-IF
           END-IF

           IF RSAL-REAJUSTE = 'N'           AND
              RSAL-REAJUSTE-PB NOT = SPACES OR
              AUX-PCALCULOPB-S NOT = ZEROES OR
              AUX-PPORCEN-REAL NOT = ZEROES
              MOVE 'POL SIN REAJUSTE PERO CON DATOS DEL MISMO'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JX99'          TO X350J000-ID-PROGRAMA
              MOVE 'B350JX99'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE '03'                TO X350J000-ID-ERROR-MOD

              PERFORM 2400-ESCRIBIR-ERRORES
           END-IF

      *    PERFORM 5214-CALCULOS-LTB
           MOVE ZEROES                 TO RSAL-PBPAGA

      *D*
      *         DISPLAY 'RSAL-FECHA CIERRE: ' WS-FECSIN-CIERRE
      *         DISPLAY 'RSAL-FECHA HASTA: ' WS-FECSIN-HASTA
      *F*
LUNA       MOVE RSAL-FCIERREA TO WS-FECSIN-ACIERRE
LUNA       MOVE RSAL-FCIERREM TO WS-FECSIN-MCIERRE
LUNA       MOVE RSAL-FCIERRED TO WS-FECSIN-DCIERRE
LUNA
LUNA       MOVE WS-FECSIN-DESDE TO RSAL-FECSIN-DESDE
LUNA       IF WS-FECSIN-CIERRE < WS-FECSIN-HASTA
LUNA          MOVE WS-FECSIN-CIERRE TO RSAL-FECSIN-HASTA
LUNA       ELSE
LUNA          MOVE WS-FECSIN-HASTA TO RSAL-FECSIN-HASTA
LUNA       END-IF.

           WRITE REG-SALIDA1 FROM RSALIDA

           MOVE ZEROES TO WS-IMPORTE-PERIOD-LTA

      *    MOVE WS-POLIZA-2 TO WS-POLIZA-2-ANT
      *    MOVE WS-SUBPOL-2 TO WS-SUBPOL-2-ANT
      *    MOVE WS-POLIZA-4 TO WS-POLIZA-4-ANT
      *    MOVE WS-SUBPOL-4 TO WS-SUBPOL-4-ANT

           INITIALIZE RSALIDA
0124  *    SET SW-NO-LTPA TO TRUE
LTA        .

      ******************************************************************10650099
       2100-RECUPERAR-DATOS-NO-RENO.                                    10660099
      ******************************************************************10670099
PBTEC
      *D*
      *    DISPLAY '2100-RECUPERAR-DATOS-NO-RENO'
      *    DISPLAY 'X350JPX3-KPOLIZA ' X350JPX3-KPOLIZA
      *    DISPLAY 'X350JPX3-KSUBPOL ' X350JPX3-KSUBPOL
      *    DISPLAY 'X350JPX3-FRENO-ADESDE ' X350JPX3-FRENO-ADESDE
      *    DISPLAY 'X350JPX3-FRENO-MDESDE ' X350JPX3-FRENO-MDESDE
      *    DISPLAY 'X350JPX3-FRENO-DDESDE ' X350JPX3-FRENO-DDESDE
      *    DISPLAY 'X350JXX2-FRENO-AHASTA ' X350JXX2-FRENO-AHASTA
      *    DISPLAY 'X350JXX2-FRENO-MHASTA ' X350JXX2-FRENO-MHASTA
      *    DISPLAY 'X350JXX2-FRENO-DHASTA ' X350JXX2-FRENO-DHASTA
      *F*
SINUSO*MF001 INITIALIZE DATOS-CTEC
MF001      INITIALIZE
                      RSAL-PRIMA-RECIBOS
                      X350JPX6-PRIMA-RECIBOS
                      RSAL-APUNTE-RECIBO
                      X350JPX6-APUNTE-RECIBO
                      RSAL-TOTAL-SINIES
                      RSAL-INGRESOS-GASTOS
                      X350JPX6-INGRESOS-GASTOS
                      RSAL-NETA-COASEGUR
                      X350JPX6-NETA-COASEGUR
                      RSAL-PB-TECNICA
                      X350JPX6-PB-TECNICA
                      RSAL-DEV-NETCOASE
                      X350JPX6-DEV-NETCOASE
                      WS-IMPORTE-REPARTOS

           MOVE '3000'                   TO RSAL-MODELO
           MOVE X350JPX3-CCANAL          TO RSAL-CANAL
                                            X350JPX6-CANAL
           MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA
           MOVE X350JPX3-KSUBPOL         TO RSAL-SUBPOLIZA
           MOVE X350JPX3-KSUSCRI         TO RSAL-SUSCRIP

           MOVE X350JPX3-FEFECTO-AINIPOL TO RSAL-FORIGENASUSCRI
           MOVE X350JPX3-FEFECTO-MINIPOL TO RSAL-FORIGENMSUSCRI
           MOVE X350JPX3-FEFECTO-DINIPOL TO RSAL-FORIGENDSUSCRI

           MOVE X350JPX3-KRAMO           TO RSAL-RAMO
                                            X350JPX6-RAMO
           MOVE X350JPX3-MODALI          TO RSAL-MODALIDAD
                                            X350JPX6-MODALIDAD
           MOVE X350JPX3-VIGOR-ANUL      TO RSAL-CESTADOASEG
           MOVE '00'                     TO RSAL-PREGRUPO(1:2)
           MOVE X350JPX3-CARTINV         TO RSAL-PREGRUPO(3:2)
           MOVE X350JPX3-CASADA          TO RSAL-CASADO
           MOVE X350JPX3-PINTER-TECBASE  TO RSAL-I1BTI
           MOVE X350JPX3-FRENO-ADESDE    TO RSAL-FRENO-ADESDE
           MOVE X350JPX3-FRENO-MDESDE    TO RSAL-FRENO-MDESDE
           MOVE X350JPX3-FRENO-DDESDE    TO RSAL-FRENO-DDESDE

           MOVE X350JXX2-FRENO-AHASTA    TO RSAL-FRENO-AHASTA
PBTECJ*    MOVE WS-MES-CNTL              TO RSAL-FRENO-MHASTA
  *****    MOVE WS-DIA-CNTL              TO RSAL-FRENO-DHASTA
           MOVE X350JXX2-FRENO-MHASTA    TO RSAL-FRENO-MHASTA
           MOVE X350JXX2-FRENO-DHASTA    TO RSAL-FRENO-DHASTA

           MOVE X350JXX2-FRENO-HASTA     TO WS-FECHA-HASTA-NR
           MOVE X350JPX3-FRENO-DESDE     TO WS-FECHA-DESDE-NR
           MOVE 'N'                  TO RSAL-RENOVADA
      *D*
      *    DISPLAY 'X350JPX3-KPOLIZA  ' X350JPX3-KPOLIZA
      *                                 X350JPX3-KSUBPOL
      *    DISPLAY 'RSAL-FRENO-DESDE  ' RSAL-FRENO-ADESDE
      *                                 RSAL-FRENO-MDESDE
      *                                 RSAL-FRENO-DDESDE
      *    DISPLAY 'RSAL-FRENO-HASTA  ' RSAL-FRENO-AHASTA
      *                                 RSAL-FRENO-MHASTA
      *                                 RSAL-FRENO-DHASTA
      *    DISPLAY 'WS-FECHA-DESDE-NR ' WS-FECHA-DESDE-NR
      *    DISPLAY 'WS-FECHA-HASTA-NR ' WS-FECHA-HASTA-NR
      *F*

           MOVE X350JXX2-FRENO-AHASTA    TO RSAL-FCIERREA
                                            X350JPX6-FCIERREA
PBTECJ*    MOVE WS-MES-CNTL              TO RSAL-FCIERREM
PBTECJ     MOVE X350JXX2-FRENO-MHASTA    TO RSAL-FCIERREM
PBTECJ                                      X350JPX6-FCIERREM
PBTECJ*    MOVE WS-DIA-CNTL              TO RSAL-FCIERRED
PBTECJ     MOVE X350JXX2-FRENO-DHASTA    TO RSAL-FCIERRED
                                            X350JPX6-FCIERRED

           MOVE X350JXX2-FRENO-AHASTA    TO WS-FECSIN-ADESDEPB
      *                                     WS-FECSIN-ADESDE
           MOVE X350JXX2-FRENO-MHASTA    TO WS-FECSIN-MDESDEPB
      *                                     WS-FECSIN-MDESDE
           MOVE X350JXX2-FRENO-DHASTA    TO WS-FECSIN-DDESDEPB
      *                                     WS-FECSIN-DDESDE

LTA        MOVE 'PBAN'                   TO RSAL-LTA
LTA        MOVE 'S'                      TO WS-ANTPBAN
           MOVE X350JXX2-FCOBRO          TO RSAL-FORMAPAGO
           MOVE X350JPX3-TIPO-COASE      TO RSAL-COASEGURO
           MOVE X350JPX3-PCOASE          TO RSAL-PCOASEGURO
           MOVE X350JPX3-PAGADOR         TO RSAL-PGLOBAL

           IF X350JPX3-CERTI EQUAL '000000' AND                         000
              X350JPX3-ASEG-MECANIZADOS = 'N'                           000
              MOVE 'S' TO RSAL-IMNOMINADA                               000
           ELSE
              MOVE 'N' TO RSAL-IMNOMINADA                               000
           END-IF

           MOVE X350JPX3-NASEGURADO      TO RSAL-NASEGURADOS

           IF X350JPX3-VIGOR-ANUL  = 'A'
              MOVE X350JPX3-ANO-ANUL     TO RSAL-FECAANULACION
              MOVE X350JPX3-MES-ANUL     TO RSAL-FECMANULACION
              MOVE X350JPX3-DIA-ANUL     TO RSAL-FECDANULACION
              MOVE X350JPX3-MOTIVO-ANU   TO RSAL-MOTIVO
           END-IF


130225     MOVE 1                        TO WS-ALFA
           MOVE WS-ALFA                  TO RSAL-ALFA

200524          IF SW-SI-LTB
200524             MOVE X350M004-KGRUPEMP-BS(3:3) TO X350JPX3-KGRUEMP
200524          END-IF

200524          IF X350JPX3-KGRUEMP = ZEROS
200524            MOVE SPACES           TO X350JPX3-KGRUEMP
200524          END-IF
           MOVE X350JPX3-KGRUEMP         TO RSAL-GEMPRESA

PBTECJ*    IF AUX-TIP-REAJUSTE = '1' OR 'T'
      *       MOVE 'S'                   TO RSAL-REAJUSTE
      *    ELSE
      *       MOVE 'N'                   TO RSAL-REAJUSTE
      *    END-IF

           MOVE AUX-CCOBON-REAJUS        TO RSAL-REAJUSTE

           MOVE AUX-REAJUSTE-PB          TO RSAL-REAJUSTE-PB
           MOVE WS-REAJREC               TO RSAL-REAJUSTE-REC
           MOVE AUX-TIP-REAJUSTE         TO RSAL-TIP-REAJUSTE
           MOVE AUX-NANIOPER             TO RSAL-APERDIDAS
           MOVE AUX-NANIOBEN             TO RSAL-ABENEFICI

           IF X350X098-IANTICIPO-S > ZEROES
              MOVE 'S'                   TO RSAL-ANTICIPO-PB
           ELSE
              MOVE 'N'                   TO RSAL-ANTICIPO-PB
           END-IF
           MOVE AUX-PANTICIPO-S          TO RSAL-P-ANTICIPO
           MOVE AUX-CCUADROBAREMO-S      TO RSAL-BAREMO
           MOVE AUX-PCALCULOPB-S         TO RSAL-P-PB-APLICAR
           MOVE AUX-PPORCEN-REAL         TO RSAL-REAJUSTEO
           MOVE AUX-PCALCULOPB-S         TO RSAL-P-REAUSTE-PB

           IF AUX-PPORCEN-REAL    = ZEROES
              MOVE AUX-PCALCULOPB-S      TO RSAL-P-PB-APLICAR
              IF AUX-CCUADROBAREMO-S EQUAL ZEROES
              MOVE SPACES                TO RSAL-BAREMO
              END-IF
              IF AUX-PCALCULOPB-S = ZEROES
                 MOVE ZEROES TO RSAL-P-PB-APLICAR
                 IF AUX-CCUADROBAREMO-S EQUAL ZEROES
                 MOVE SPACES              TO RSAL-BAREMO
                 END-IF
              END-IF
           ELSE
              MOVE AUX-PPORCEN-REAL      TO RSAL-P-PB-APLICAR

           END-IF

           IF AUX-PPORCEN-REAL NOT = ZEROES AND
              AUX-PCALCULOPB-S NOT = ZEROES
              MOVE AUX-CCUADROBAREMO-S TO RSAL-BAREMO
PBTEC         MOVE AUX-PCALCULOPB-S    TO RSAL-P-PB-APLICAR
           END-IF

           MOVE AUX-PGASTOS-S            TO RSAL-P-GASTOS-PB
PBTEC      MOVE WS-REAJREC               TO RSAL-REAJUSTE-REC

      *    COMPUTE RSAL-PRIMA-ANUAL ROUNDED = WS-IMPORTE-PRI
josen *    COMPUTE RSAL-PRIMA-ANUAL ROUNDED = X350JXX2-EPNETA
josen      COMPUTE RSAL-PRIMA-ANUAL ROUNDED = X350JPX3-EPNETA

jose16     MOVE X350JPX3-EPNETA          TO X350JPX6-PRIMA-ANUAL
           MOVE ZEROES                   TO RSAL-GASTOS
                                            X350JPX6-GASTOS
           MOVE X350X098-IARRASTRE-S     TO RSAL-IMP-ARRAS-PERD
           MOVE ZEROES                   TO RSAL-NETA-COASEGUR
                                        X350JPX6-NETA-COASEGUR

           IF X350JPX3-KGRUEMP = '000'
              MOVE SPACES           TO X350JPX3-KGRUEMP
           END-IF

           IF X350JPX3-KGRUEMP NOT = SPACES
             MOVE X350JPX4-NASEGURADO    TO RSAL-ASE-TOT-EMPRESA
           END-IF

      *D*
      *    DISPLAY 'X350JPX3-KGRUEMP ' X350JPX3-KGRUEMP
      *F*
                IF X350JPX3-KGRUEMP NOT = SPACES
      *D*
      *           DISPLAY 'PASO IF'
      *           DISPLAY 'X350JPX3-KGRUEMP ' X350JPX3-KGRUEMP
      *F*
                  OPEN INPUT ENTRADA-TASE
                  PERFORM 2703-LEER-TOTALES-ASEGURADO
                    UNTIL X350JPX3-KGRUEMP EQUAL
                          X350JPX4-KGRUEMP
                  CLOSE ENTRADA-TASE

      *D*
      *           DISPLAY 'LEE CORRECTAMENTE TASE'
      *F*
                  OPEN INPUT ENTRADA-TASE-CP
                  PERFORM 2703-LEER-TOTALES-ASEGURADO-CP
                    UNTIL X350JPX3-KPOLIZA EQUAL
                          X350JPX5-KPOLIZA AND
                          X350JPX3-KSUBPOL EQUAL
                          X350JPX5-KSUBPOL
                  CLOSE ENTRADA-TASE-CP

      *D*
      *           DISPLAY 'LEE CORRECTAMENTE TASE-CP'
      *           DISPLAY 'X350JPX5-PRIMA-ANUAL ' X350JPX5-PRIMA-ANUAL
      *F*
                  IF X350JPX5-PRIMA-ANUAL <= ZEROES
                     MOVE ZEROES TO WS-IMPORTE-REPARTOS
                  ELSE

      *D*
      *          DISPLAY 'VA POR ELSE'
      *          DISPLAY 'X350JPX4-PERIOD-NR ' X350JPX4-PERIOD-NR
      *          DISPLAY 'X350JPX4-SINIES-NR   ' X350JPX4-SINIES-NR
      *          DISPLAY 'X350JPX4-GASTOS-NR   ' X350JPX4-GASTOS-NR
      *          DISPLAY 'X350JPX5-PRIMA-ANUAL ' X350JPX5-PRIMA-ANUAL
      *          DISPLAY 'X350JPX4-PRIMA-ANUAL ' X350JPX4-PRIMA-ANUAL
      *F*

PBTEC *2701      COMPUTE WS-IMPORTE-REPARTOS    =
PBTEC *2701                           ((X350JPX4-PERIOD-NR -
PBTEC *2701                            X350JPX4-SINIES-NR  -
PBTEC *2701                            X350JPX4-GASTOS-NR) *
PBTEC *2701                            X350JPX5-PRIMA-ANUAL) /
PBTEC *2701                            X350JPX4-PRIMA-ANUAL

2701                 IF X350JPX6-FCIERREA = R350T000-FAFINAL AND
2701                    X350JPX6-FCIERREM = R350T000-FMFINAL AND
2701                    X350JPX6-FCIERRED = R350T000-FDFINAL
2701                       COMPUTE WS-IMPORTE-REPARTOS    =
2701                                            ((X350JPX4-PERIOD    -
2701                                              X350JPX4-SINIES     -
2701                                              X350JPX4-GASTOS   ) *
2701                                             X350JPX5-PRIMA-ANUAL) /
2701                                             X350JPX4-PRIMA-ANUAL
2701                    ELSE
2701                       COMPUTE WS-IMPORTE-REPARTOS    =
2701                                            ((X350JPX4-PERIOD-NR -
2701                                              X350JPX4-SINIES-NR  -
2701                                              X350JPX4-GASTOS-NR) *
2701                                             X350JPX5-PRIMA-ANUAL) /
2701                                             X350JPX4-PRIMA-ANUAL-NR
2701                    END-IF
051124*2701      IF X350JPX3-PAGADOR = '0'
051124*2701         COMPUTE WS-IMPORTE-REPARTOS    =
051124*2701                              ((X350JPX4-PERIOD    -
051124*2701                                X350JPX4-SINIES     -
051124*2701                                X350JPX4-GASTOS   ) *
051124*2701                                X350JPX5-PRIMA-ANUAL) /
051124*2701                                X350JPX4-PRIMA-ANUAL
051124*2701      END-IF

                 END-IF

      *D*
      *         DISPLAY 'WS-IMPORTE-REPARTOS 1  ' WS-IMPORTE-REPARTOS
      *         DISPLAY 'X350JPX4-PERIOD        ' X350JPX4-PERIOD
      *         DISPLAY 'X350JPX4-SINIES        ' X350JPX4-SINIES
      *         DISPLAY 'X350JPX4-GASTOS        ' X350JPX4-GASTOS
      *         DISPLAY 'X350JPX4-PERIOD-NR     ' X350JPX4-PERIOD-NR
      *         DISPLAY 'X350JPX4-SINIES-NR     ' X350JPX4-SINIES-NR
      *         DISPLAY 'X350JPX4-GASTOS-NR     ' X350JPX4-GASTOS-NR
      *         DISPLAY 'X350JPX5-PRIMA-ANUAL   ' X350JPX5-PRIMA-ANUAL
      *         DISPLAY 'X350JPX4-PRIMA-ANUAL   ' X350JPX4-PRIMA-ANUAL
      *
      *F*
                  MOVE WS-IMPORTE-REPARTOS
                                           TO  X350JPX6-INGRESOS-GASTOS

                  IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
                     MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
                  END-IF

                  COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
                                               = WS-IMPORTE-REPARTOS
      *D*
      *         DISPLAY 'WS-IMPORTE-REPARTOS 2  '
      *                                         WS-IMPORTE-REPARTOS
      *F*

                ELSE
      *D*
      *        DISPLAY 'ME VOY POR ELSE NO RENO'
      *        DISPLAY 'X350JPX4-KPOLIZA       ' X350JPX4-KPOLIZA
      *        DISPLAY 'X350JPX3-KPOLIZA       ' X350JPX3-KPOLIZA
      *        DISPLAY 'X350JPX3-KSUBPOL       ' X350JPX3-KSUBPOL
      *        DISPLAY 'X350JPX4-KSUBPOL       ' X350JPX4-KSUBPOL
      *        DISPLAY 'X350JPX3-KGRUEMP       ' X350JPX3-KGRUEMP
      *        DISPLAY 'X350JPX4-KGRUEMP       ' X350JPX4-KGRUEMP
      *F*
                  OPEN INPUT ENTRADA-TASE-NE
                  PERFORM 2703-LEER-TOTALES-ASEGURADO-NE
                    UNTIL X350JPX3-KPOLIZA EQUAL
                          X350JPX4-KPOLIZA AND
                          X350JPX3-KSUBPOL EQUAL
                          X350JPX4-KSUBPOL AND
                          X350JPX3-KGRUEMP EQUAL
                          X350JPX4-KGRUEMP
                  CLOSE ENTRADA-TASE-NE

      *D*
      *           DISPLAY 'LEE CORRECTAMENTE TASE-NE'
      *F*

                  OPEN INPUT ENTRADA-TASE-CP
                  PERFORM 2703-LEER-TOTALES-ASEGURADO-CP
                    UNTIL X350JPX3-KPOLIZA EQUAL
                          X350JPX5-KPOLIZA AND
                          X350JPX3-KSUBPOL EQUAL
                          X350JPX5-KSUBPOL
                  CLOSE ENTRADA-TASE-CP

                  IF X350JPX5-PRIMA-ANUAL <= ZEROES
                     MOVE ZEROES TO WS-IMPORTE-REPARTOS
                  ELSE
                  COMPUTE WS-IMPORTE-REPARTOS = ((WS-IMPORTE-PERIOD -
060326*                                         WS-IMPORTE-206    -
060326                                          TB-ACUMULADO-206 (2) -
                                                WS-IMP-GASTOS) *
                                                X350JPX5-PRIMA-ANUAL) /
                                                 X350JPX5-PRIMA-ANUAL

                  END-IF
                  MOVE WS-IMPORTE-REPARTOS TO X350JPX6-INGRESOS-GASTOS

                  IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
                     MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
                  END-IF

                  COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
                                               = WS-IMPORTE-REPARTOS

                END-IF

                COMPUTE WS-NETA-COASEGUR ROUNDED
                                         = (WS-IMPORTE-REPARTOS *
                                            X350JPX3-PCOASE) / 100
                MOVE WS-NETA-COASEGUR         TO X350JPX6-NETA-COASEGUR

                IF X350JPX6-NETA-COASEGUR NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-NETA-COASEGUR
                END-IF

                COMPUTE RSAL-NETA-COASEGUR ROUNDED
                                           = WS-NETA-COASEGUR

                MOVE RSAL-P-PB-APLICAR  TO AUX-PB-TECNICA

                COMPUTE RSAL-PB-TECNICA ROUNDED
                                        = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100
                COMPUTE X350JPX6-PB-TECNICA = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100

                IF X350JPX6-PB-TECNICA NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-PB-TECNICA
                END-IF

                MOVE X350JPX3-PCOASE          TO AUX-PCOASE

                COMPUTE X350JPX6-DEV-NETCOASE = (WS-NETA-COASEGUR *
                                       AUX-PB-TECNICA) / 100

                IF X350JPX6-DEV-NETCOASE < ZEROES
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                   MOVE ZEROES TO RSAL-DEV-NETCOASE
                ELSE
      *D*
      *         DISPLAY 'DISPLAY LTB'
      *         DISPLAY 'WS-NETA-COASEGUR ' WS-NETA-COASEGUR
      *         DISPLAY 'AUX-PB-TECNICA ' AUX-PB-TECNICA
      *F*
                   COMPUTE RSAL-DEV-NETCOASE ROUNDED
                                          = (WS-NETA-COASEGUR *
                                          AUX-PB-TECNICA) / 100
                END-IF

                IF X350JPX6-DEV-NETCOASE NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                END-IF

      *D*
      *         DISPLAY 'RSAL-DEV-NETCOASE ' RSAL-DEV-NETCOASE
      *F*
           IF RSAL-PGLOBAL = '0'
              MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA-REF1
              MOVE '/'                      TO RSAL-SEPARADOR
              MOVE '0000'                   TO RSAL-SUBPOL-REF1
           END-IF

           IF RSAL-POLIZA-REF1 = '0000000'
              MOVE SPACES TO RSAL-POLIZA-REF1
              MOVE SPACES TO RSAL-SEPARADOR
              MOVE SPACES TO RSAL-SUBPOL-REF1
           END-IF

           PERFORM 2210-RECUPERAR-UOA-NR
           PERFORM 2300-OBTENER-NEW-GAP

LUNA       MOVE RSAL-FCIERREA TO WS-FECSIN-ACIERRE
LUNA       MOVE RSAL-FCIERREM TO WS-FECSIN-MCIERRE
LUNA       MOVE RSAL-FCIERRED TO WS-FECSIN-DCIERRE
LUNA
LUNN       IF WS-RENOVADA = 'N' AND WS-ANTPBAN = 'S' AND RSAL-LTA = 'PB'
LUNN          MOVE WS-FECSIN-DESDE-PB TO WS-FECSIN-DESDE
           END-IF

LUNA       MOVE WS-FECSIN-DESDE TO RSAL-FECSIN-DESDE
      *D*
      *    DISPLAY 'WS-FECSIN-DESDE ' WS-FECSIN-DESDE
      *F*
LUNA       IF WS-FECSIN-CIERRE < WS-FECSIN-HASTA
LUNA          MOVE WS-FECSIN-CIERRE TO RSAL-FECSIN-HASTA
LUNA       ELSE
LUNA          MOVE WS-FECSIN-HASTA TO RSAL-FECSIN-HASTA
LUNA       END-IF

           IF RSAL-REAJUSTE = 'S' AND  RSAL-REAJUSTE-PB = SPACES
              MOVE 'POL CON REAJUSTE CON SU TIPO PENDIENTE'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'B350JA12'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE '01'                TO X350J000-ID-ERROR-MOD
              PERFORM 2400-ESCRIBIR-ERRORES
           ELSE
              IF RSAL-REAJUSTE = 'S' AND
                 RSAL-REAJUSTE-PB NOT = SPACES
                 MOVE 'POL CON REAJUSTE CON SU TIPO INCONGRUENTE'
                                           TO X350J000-DESC-ERROR
                  MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                  MOVE 'B350JA12'          TO X350J000-ID-MODULO
                  MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                  MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                  MOVE 'W'                 TO X350J000-NIVEL
                  MOVE '02'                TO X350J000-ID-ERROR-MOD
                  PERFORM 2400-ESCRIBIR-ERRORES
                END-IF
           END-IF

           IF RSAL-REAJUSTE = 'N'           AND
              RSAL-REAJUSTE-PB NOT = SPACES OR
              AUX-PCALCULOPB-S NOT = ZEROES OR
              AUX-PPORCEN-REAL NOT = ZEROES
              MOVE 'POL SIN REAJUSTE PERO CON DATOS DEL MISMO'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'B350JA12'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE '03'                TO X350J000-ID-ERROR-MOD

              PERFORM 2400-ESCRIBIR-ERRORES
           END-IF

      *    MOVE WS-IMPORTE-PBP TO RSAL-PBPAGA
           .
      *                                                                 10890099
      ******************************************************************10650099
       2200-RECUPERAR-UOA.                                              10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2200-RECUPERAR-UOA'
      *F*

           INITIALIZE  WS-X340M0N2

           SET SW-SI-PRIMERA-VEZ             TO TRUE
           SET SW-NO-ENCONTRADO      TO TRUE
           SET SW-NO-ERROR           TO TRUE

           IF SW-SI-PRIMERA-VEZ
              MOVE 'BL'                      TO X340M0N2-OPERACION-E
              MOVE '000'               TO X340M0N2-KCOMPANIA-E(1:3)
              MOVE X350JXX2-CCANAL      TO X340M0N2-KCOMPANIA-E(4:2)

              IF X350JXX2-MODALI = 400
                 MOVE X350JXX2-MODALI-ORIGEN TO X340M0N2-KMODALIDAD-E

                 IF X350JXX2-MODALI-ORIGEN = WS-MODALIDAD-ORIGEN
                    MOVE WS-RAMO-ORIGEN      TO X340M0N2-KRAMO-E

                 ELSE
                    PERFORM 2220-RECUPERAR-RAMO-ORIGEN

                    MOVE WS-RAMO-ORIGEN      TO X340M0N2-KRAMO-E

                 END-IF

              ELSE
                 MOVE '00'                    TO X340M0N2-KRAMO-E(1:2)
                 MOVE X350JXX2-KRAMO          TO X340M0N2-KRAMO-E(3:3)
                 MOVE X350JXX2-MODALI         TO X340M0N2-KMODALIDAD-E

              END-IF

              MOVE X350JXX2-CASADA              TO X340M0N2-SWCASADO-E

              SET SW-NO-PRIMERA-VEZ             TO TRUE
           ELSE
              MOVE 'BR'                      TO X340M0N2-OPERACION-E

           END-IF

           CALL B340M0N2 USING WS-X340M0N2

           EVALUATE X340M0N2-CRETORNO-S
               WHEN '00'
               PERFORM VARYING I FROM 1 BY 1
                 UNTIL (X340M0N2-KCOMPANIA-S(I) = SPACES
                    OR LOW-VALUES) OR SW-SI-ENCONTRADO OR SW-SI-ERROR

                       EVALUATE X340M0N2-METMEDICION-S(I)(1:1)
                           WHEN 'P'
                                IF (X350JXX2-FRENO-DESDE  >=
                                   X340M0N2-FECDESDE-S(I)) AND
                                   (X350JXX2-FRENO-DESDE  <=
                                   X340M0N2-FECHASTA-S(I))
                                   IF X340M0N2-SWPOLONER-S(I) = 'S'
                                      SET SW-SI-PRIMERA-VEZ  TO TRUE
                                      PERFORM 2210-RECUPERAR-EXCEP-UMIC
                                        UNTIL SW-SI-ENCONTRADO
                                           OR SW-SI-ERROR OR
                                           X340M0N4-SPAGINACION-S = 'N'

                                   ELSE
                                      MOVE X340M0N2-UOA-S(I)
                                                 TO RSAL-UOACOUNT
                                                 X350JPX6-UOACOUNT

                                      SET SW-SI-ENCONTRADO TO TRUE

                                   END-IF

                                END-IF

                           WHEN 'V'
                                IF (X350JXX2-FEFECTO-INIPOL >=
                                   X340M0N2-FECDESDE-S(I)) AND
                                   (X350JXX2-FEFECTO-INIPOL <=
                                   X340M0N2-FECHASTA-S(I))
                                   IF X340M0N2-SWPOLONER-S(I) = 'S'
                                      SET SW-SI-PRIMERA-VEZ  TO TRUE

                                      PERFORM 2210-RECUPERAR-EXCEP-UMIC
                                        UNTIL SW-SI-ENCONTRADO
                                           OR SW-SI-ERROR OR
                                          X340M0N4-SPAGINACION-S = 'N'

                                   ELSE
                                      MOVE X340M0N2-UOA-S(I)
                                                 TO RSAL-UOACOUNT
                                                 X350JPX6-UOACOUNT
                                      SET SW-SI-ENCONTRADO TO TRUE
                                   END-IF

                                END-IF

                           WHEN 'B'
                                IF (X350JXX2-FEFECTO-INIPOL >=
                                   X340M0N2-FECDESDE-S(I)) AND
                                   (X350JXX2-FEFECTO-INIPOL <=
                                   X340M0N2-FECHASTA-S(I))
                                   IF X340M0N2-SWPOLONER-S(I) = 'S'
                                      SET SW-SI-PRIMERA-VEZ  TO TRUE

                                      PERFORM 2210-RECUPERAR-EXCEP-UMIC
                                        UNTIL SW-SI-ENCONTRADO
                                           OR SW-SI-ERROR OR
                                            X340M0N4-SPAGINACION-S = 'N'

                                   ELSE
                                      MOVE X340M0N2-UOA-S(I)
                                                 TO RSAL-UOACOUNT
                                                 X350JPX6-UOACOUNT

                                      SET SW-SI-ENCONTRADO TO TRUE

                                   END-IF

                                END-IF

                           WHEN OTHER
                                CONTINUE

                       END-EVALUATE

               END-PERFORM

               WHEN '05'
                    MOVE 'ERROR AL RECUPERAR UNIT OF ACCOUNT NIIF17'
                                             TO X350J000-DESC-ERROR
                    MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                    MOVE 'B340M0N2'          TO X350J000-ID-MODULO
                    MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                    MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                    MOVE 'W'                 TO X350J000-NIVEL
                    MOVE X340M0N2-CRETORNO-S TO X350J000-ID-ERROR-MOD

                    PERFORM 2400-ESCRIBIR-ERRORES

               WHEN OTHER
                    MOVE 'ERROR AL RECUPERAR UNIT OF ACCOUNT NIIF17'
                                             TO X350J000-DESC-ERROR
                    MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                    MOVE 'B340M0N2'          TO X350J000-ID-MODULO
                    MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                    MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                    MOVE 'W'                 TO X350J000-NIVEL
                    MOVE X340M0N2-CRETORNO-S TO X350J000-ID-ERROR-MOD

                    PERFORM 2400-ESCRIBIR-ERRORES

      *
           END-EVALUATE
              .
      *                                                                 10890099
      *                                                                 10890099
      ******************************************************************10650099
       2210-RECUPERAR-UOA-NR.                                           10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2210-RECUPERAR-UOA-NR'
      *F*

           INITIALIZE  WS-X340M0N2

           SET SW-SI-PRIMERA-VEZ             TO TRUE
           SET SW-NO-ENCONTRADO      TO TRUE
           SET SW-NO-ERROR           TO TRUE

           IF SW-SI-PRIMERA-VEZ
              MOVE 'BL'                      TO X340M0N2-OPERACION-E
              MOVE '000'               TO X340M0N2-KCOMPANIA-E(1:3)
              MOVE X350JPX3-CCANAL      TO X340M0N2-KCOMPANIA-E(4:2)

              IF X350JPX3-MODALI = 400
                 MOVE X350JPX3-MODALI-ORIGEN TO X340M0N2-KMODALIDAD-E

                 IF X350JPX3-MODALI-ORIGEN = WS-MODALIDAD-ORIGEN
                    MOVE WS-RAMO-ORIGEN      TO X340M0N2-KRAMO-E

                 ELSE
                    PERFORM 2221-RECUPERAR-RAMO-ORIGEN-NR

                    MOVE WS-RAMO-ORIGEN      TO X340M0N2-KRAMO-E

                 END-IF

              ELSE
                 MOVE '00'                    TO X340M0N2-KRAMO-E(1:2)
                 MOVE X350JPX3-KRAMO          TO X340M0N2-KRAMO-E(3:3)
                 MOVE X350JPX3-MODALI         TO X340M0N2-KMODALIDAD-E

              END-IF

              MOVE X350JPX3-CASADA              TO X340M0N2-SWCASADO-E

              SET SW-NO-PRIMERA-VEZ             TO TRUE
           ELSE
              MOVE 'BR'                      TO X340M0N2-OPERACION-E

           END-IF

           CALL B340M0N2 USING WS-X340M0N2

           EVALUATE X340M0N2-CRETORNO-S
               WHEN '00'
               PERFORM VARYING I FROM 1 BY 1
                 UNTIL (X340M0N2-KCOMPANIA-S(I) = SPACES
                    OR LOW-VALUES) OR SW-SI-ENCONTRADO OR SW-SI-ERROR

                       EVALUATE X340M0N2-METMEDICION-S(I)(1:1)
                           WHEN 'P'
                                IF (X350JPX3-FRENO-DESDE  >=
                                   X340M0N2-FECDESDE-S(I)) AND
                                   (X350JPX3-FRENO-DESDE  <=
                                   X340M0N2-FECHASTA-S(I))
                                   IF X340M0N2-SWPOLONER-S(I) = 'S'
                                      SET SW-SI-PRIMERA-VEZ  TO TRUE
                                   PERFORM 2210-RECUPERAR-EXCEP-UMIC-NR
                                        UNTIL SW-SI-ENCONTRADO
                                           OR SW-SI-ERROR OR
                                           X340M0N4-SPAGINACION-S = 'N'

                                   ELSE
                                      MOVE X340M0N2-UOA-S(I)
                                                 TO RSAL-UOACOUNT
                                                 X350JPX6-UOACOUNT

                                      SET SW-SI-ENCONTRADO TO TRUE

                                   END-IF

                                END-IF

                           WHEN 'V'
                                IF (X350JPX3-FEFECTO-INIPOL >=
                                   X340M0N2-FECDESDE-S(I)) AND
                                   (X350JPX3-FEFECTO-INIPOL <=
                                   X340M0N2-FECHASTA-S(I))
                                   IF X340M0N2-SWPOLONER-S(I) = 'S'
                                      SET SW-SI-PRIMERA-VEZ  TO TRUE

                                   PERFORM 2210-RECUPERAR-EXCEP-UMIC-NR
                                        UNTIL SW-SI-ENCONTRADO
                                           OR SW-SI-ERROR OR
                                          X340M0N4-SPAGINACION-S = 'N'

                                   ELSE
                                      MOVE X340M0N2-UOA-S(I)
                                                 TO RSAL-UOACOUNT
                                                 X350JPX6-UOACOUNT
                                      SET SW-SI-ENCONTRADO TO TRUE
                                   END-IF

                                END-IF

                           WHEN 'B'
                                IF (X350JPX3-FEFECTO-INIPOL >=
                                   X340M0N2-FECDESDE-S(I)) AND
                                   (X350JPX3-FEFECTO-INIPOL <=
                                   X340M0N2-FECHASTA-S(I))
                                   IF X340M0N2-SWPOLONER-S(I) = 'S'
                                      SET SW-SI-PRIMERA-VEZ  TO TRUE

                                   PERFORM 2210-RECUPERAR-EXCEP-UMIC-NR
                                        UNTIL SW-SI-ENCONTRADO
                                           OR SW-SI-ERROR OR
                                            X340M0N4-SPAGINACION-S = 'N'

                                   ELSE
                                      MOVE X340M0N2-UOA-S(I)
                                                 TO RSAL-UOACOUNT
                                                 X350JPX6-UOACOUNT

                                      SET SW-SI-ENCONTRADO TO TRUE

                                   END-IF

                                END-IF

                           WHEN OTHER
                                CONTINUE

                       END-EVALUATE

               END-PERFORM

               WHEN '05'
                    SET SW-SI-ERROR        TO TRUE

                    MOVE 'ERROR AL RECUPERAR UNIT OF ACCOUNT NIIF17'
                                             TO X350J000-DESC-ERROR
                    MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                    MOVE 'B340M0N2'          TO X350J000-ID-MODULO
                    MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                    MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                    MOVE 'W'                 TO X350J000-NIVEL
                    MOVE X340M0N2-CRETORNO-S TO X350J000-ID-ERROR-MOD

                    PERFORM 2400-ESCRIBIR-ERRORES

               WHEN OTHER
                    SET SW-SI-ERROR        TO TRUE

                    MOVE 'ERROR AL RECUPERAR UNIT OF ACCOUNT NIIF17'
                                             TO X350J000-DESC-ERROR
                    MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                    MOVE 'B340M0N2'          TO X350J000-ID-MODULO
                    MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                    MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                    MOVE 'W'                 TO X350J000-NIVEL
                    MOVE X340M0N2-CRETORNO-S TO X350J000-ID-ERROR-MOD

                    PERFORM 2400-ESCRIBIR-ERRORES

           END-EVALUATE
              .
      *                                                                 10890099
      **************************************************************
       2210-RECUPERAR-EXCEP-UMIC.
      **************************************************************

           IF SW-SI-PRIMERA-VEZ
              MOVE 'BL'                      TO X340M0N4-OPERACION-E
              MOVE '000'             TO X340M0N4-KCOMPANIA-E(1:3)
              MOVE X350JXX2-CCANAL        TO X340M0N4-KCOMPANIA-E(4:2)

           IF X350JXX2-MODALI = 400
              MOVE X350JXX2-MODALI-ORIGEN    TO X340M0N4-KMODALIDAD-E
              MOVE WS-RAMO-ORIGEN            TO X340M0N4-KRAMO-E

           ELSE
              MOVE X350JXX2-MODALI           TO X340M0N4-KMODALIDAD-E
              MOVE '00'                      TO X340M0N4-KRAMO-E(1:2)
              MOVE X350JXX2-KRAMO            TO X340M0N4-KRAMO-E(3:3)

           END-IF

           MOVE X350JXX2-KPOLIZA             TO X340M0N4-KPOLIZA-E
           MOVE X350JXX2-KSUBPOL             TO X340M0N4-KSUBPOLIZA-E
           MOVE X350JXX2-KSUSCRI             TO X340M0N4-NSUSCRI-E
              SET SW-NO-PRIMERA-VEZ             TO TRUE
           ELSE
              MOVE 'BR'                      TO X340M0N4-OPERACION-E

           END-IF


           CALL B340M0N4 USING WS-X340M0N4

           EVALUATE X340M0N4-CRETORNO-S
               WHEN '00'
               PERFORM VARYING I FROM 1 BY 1
                 UNTIL X340M0N4-KCOMPANIA-S(I) = SPACES OR LOW-VALUES
                    OR SW-SI-ENCONTRADO
                       EVALUATE X340M0N4-METMEDICION-S(I)(1:1)
                           WHEN 'P'
                                IF (X350JXX2-FRENO-DESDE >=
                                   X340M0N4-FECDESDE-S(I)) AND
                                   (X350JXX2-FRENO-DESDE <=
                                   X340M0N4-FECHASTA-S(I))
                                      MOVE X340M0N4-UOA-S(I)
                                                 TO RSAL-UOACOUNT
                                                 X350JPX6-UOACOUNT
                                      SET SW-SI-ENCONTRADO TO TRUE
                                END-IF

                           WHEN 'V'
                                IF (X350JXX2-FEFECTO-INIPOL >=
                                   X340M0N4-FECDESDE-S(I)) AND
                                   (X350JXX2-FEFECTO-INIPOL <=
                                   X340M0N4-FECHASTA-S(I))
                                      MOVE X340M0N4-UOA-S(I)
                                                 TO RSAL-UOACOUNT
                                                 X350JPX6-UOACOUNT
                                      SET SW-SI-ENCONTRADO TO TRUE

                                END-IF

                           WHEN 'B'
                                IF (X350JXX2-FEFECTO-INIPOL >=
                                   X340M0N4-FECDESDE-S(I)) AND
                                   (X350JXX2-FEFECTO-INIPOL <=
                                   X340M0N4-FECHASTA-S(I))
                                      MOVE X340M0N4-UOA-S(I)
                                                 TO RSAL-UOACOUNT
                                                 X350JPX6-UOACOUNT
                                      SET SW-SI-ENCONTRADO TO TRUE

                                END-IF

                           WHEN OTHER
                                CONTINUE

                       END-EVALUATE

               END-PERFORM

               WHEN '05'
                    SET SW-SI-ERROR        TO TRUE

                    MOVE 'ERROR AL RECUPERAR EXCEPCION POLIZA'
                                             TO X350J000-DESC-ERROR
                    MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                    MOVE 'B340M0N4'          TO X350J000-ID-MODULO
                    MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                    MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                    MOVE 'W'                 TO X350J000-NIVEL
                    MOVE X340M0N4-CRETORNO-S TO X350J000-ID-ERROR-MOD

                    PERFORM 2400-ESCRIBIR-ERRORES

               WHEN OTHER
                    SET SW-SI-ERROR        TO TRUE

                    MOVE 'ERROR AL RECUPERAR EXCEPCION POLIZA NIIF17'
                                             TO X350J000-DESC-ERROR
                    MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                    MOVE 'B340M0N4'          TO X350J000-ID-MODULO
                    MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                    MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                    MOVE 'W'                 TO X350J000-NIVEL
                    MOVE X340M0N4-CRETORNO-S TO X350J000-ID-ERROR-MOD

                    PERFORM 2400-ESCRIBIR-ERRORES

           END-EVALUATE
      *
           .
      *
      **************************************************************
       2210-RECUPERAR-EXCEP-UMIC-NR.
      **************************************************************

           IF SW-SI-PRIMERA-VEZ
              MOVE 'BL'                      TO X340M0N4-OPERACION-E
              MOVE '000'             TO X340M0N4-KCOMPANIA-E(1:3)
              MOVE X350JPX3-CCANAL        TO X340M0N4-KCOMPANIA-E(4:2)

           IF X350JXX2-MODALI = 400
              MOVE X350JPX3-MODALI-ORIGEN    TO X340M0N4-KMODALIDAD-E
              MOVE WS-RAMO-ORIGEN            TO X340M0N4-KRAMO-E

           ELSE
              MOVE X350JPX3-MODALI           TO X340M0N4-KMODALIDAD-E
              MOVE '00'                      TO X340M0N4-KRAMO-E(1:2)
              MOVE X350JPX3-KRAMO            TO X340M0N4-KRAMO-E(3:3)

           END-IF

           MOVE X350JPX3-KPOLIZA             TO X340M0N4-KPOLIZA-E
           MOVE X350JPX3-KSUBPOL             TO X340M0N4-KSUBPOLIZA-E
           MOVE X350JPX3-KSUSCRI             TO X340M0N4-NSUSCRI-E
              SET SW-NO-PRIMERA-VEZ             TO TRUE
           ELSE
              MOVE 'BR'                      TO X340M0N4-OPERACION-E

           END-IF


           CALL B340M0N4 USING WS-X340M0N4

           EVALUATE X340M0N4-CRETORNO-S
               WHEN '00'
               PERFORM VARYING I FROM 1 BY 1
                 UNTIL X340M0N4-KCOMPANIA-S(I) = SPACES OR LOW-VALUES
                    OR SW-SI-ENCONTRADO
                       EVALUATE X340M0N4-METMEDICION-S(I)(1:1)
                           WHEN 'P'
                                IF (X350JPX3-FRENO-DESDE >=
                                   X340M0N4-FECDESDE-S(I)) AND
                                   (X350JPX3-FRENO-DESDE <=
                                   X340M0N4-FECHASTA-S(I))
                                      MOVE X340M0N4-UOA-S(I)
                                                 TO RSAL-UOACOUNT
                                                 X350JPX6-UOACOUNT
                                      SET SW-SI-ENCONTRADO TO TRUE
                                END-IF

                           WHEN 'V'
                                IF (X350JPX3-FEFECTO-INIPOL >=
                                   X340M0N4-FECDESDE-S(I)) AND
                                   (X350JPX3-FEFECTO-INIPOL <=
                                   X340M0N4-FECHASTA-S(I))
                                      MOVE X340M0N4-UOA-S(I)
                                                 TO RSAL-UOACOUNT
                                                 X350JPX6-UOACOUNT
                                      SET SW-SI-ENCONTRADO TO TRUE

                                END-IF

                           WHEN 'B'
                                IF (X350JPX3-FEFECTO-INIPOL >=
                                   X340M0N4-FECDESDE-S(I)) AND
                                   (X350JPX3-FEFECTO-INIPOL <=
                                   X340M0N4-FECHASTA-S(I))
                                      MOVE X340M0N4-UOA-S(I)
                                                 TO RSAL-UOACOUNT
                                                 X350JPX6-UOACOUNT
                                      SET SW-SI-ENCONTRADO TO TRUE

                                END-IF

                           WHEN OTHER
                                CONTINUE

                       END-EVALUATE

               END-PERFORM

               WHEN '05'
                    SET SW-SI-ERROR        TO TRUE
      *
                    MOVE 'ERROR AL RECUPERAR EXCEPCION POLIZA NIIF17'
                                             TO X350J000-DESC-ERROR
                    MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                    MOVE 'B340M0N4'          TO X350J000-ID-MODULO
                    MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                    MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                    MOVE 'W'                 TO X350J000-NIVEL
                    MOVE X340M0N4-CRETORNO-S TO X350J000-ID-ERROR-MOD

                    PERFORM 2400-ESCRIBIR-ERRORES

               WHEN OTHER
                    SET SW-SI-ERROR        TO TRUE
      *
                    MOVE 'ERROR AL RECUPERAR EXCEPCION POLIZA NIIF17'
                                             TO X350J000-DESC-ERROR
                    MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                    MOVE 'B340M0N4'          TO X350J000-ID-MODULO
                    MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                    MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                    MOVE 'W'                 TO X350J000-NIVEL
                    MOVE X340M0N4-CRETORNO-S TO X350J000-ID-ERROR-MOD

                    PERFORM 2400-ESCRIBIR-ERRORES

      *
           END-EVALUATE
      *
           .
      *
      **************************************************************
       2220-RECUPERAR-RAMO-ORIGEN.
      **************************************************************

           INITIALIZE WS-X880M007

           MOVE 'LA'                   TO  X880M007-OPERACION-E
           MOVE X350JXX2-MODALI-ORIGEN  TO  X880M007-KMODALIDAD-E

           CALL B880M007  USING  WS-X880M007

              IF X880M007-CRETORNO-S NOT = '00'
      *-->       No se encuentra la modalidad
                 MOVE 'NO SE HA ENCONTRADO RAMO ORIGEN'
                                             TO X350J000-DESC-ERROR
                 MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                 MOVE 'B880M007'          TO X350J000-ID-MODULO
                 MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                 MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                 MOVE 'W'                 TO X350J000-NIVEL
                 MOVE X880M007-CRETORNO-S TO X350J000-ID-ERROR-MOD

                 PERFORM 2400-ESCRIBIR-ERRORES

                 MOVE SPACES             TO WS-RAMO-ORI
                                            WS-MODALIDAD-ORIGEN
      *
              ELSE
                  MOVE X880M007-KRAMO-S(1)  TO WS-RAMO-ORI
                  MOVE X350JXX2-MODALI-ORIGEN  TO WS-MODALIDAD-ORIGEN
      *
              END-IF
           .
      **************************************************************
       2221-RECUPERAR-RAMO-ORIGEN-NR.
      **************************************************************

           INITIALIZE WS-X880M007

           MOVE 'LA'                   TO  X880M007-OPERACION-E
           MOVE X350JPX3-MODALI-ORIGEN  TO  X880M007-KMODALIDAD-E

           CALL B880M007  USING  WS-X880M007

              IF X880M007-CRETORNO-S NOT = '00'
      *-->       No se encuentra la modalidad
                 MOVE 'NO SE HA ENCONTRADO RAMO ORIGEN'
                                             TO X350J000-DESC-ERROR
                 MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                 MOVE 'B880M007'          TO X350J000-ID-MODULO
                 MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                 MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                 MOVE 'W'                 TO X350J000-NIVEL
                 MOVE X880M007-CRETORNO-S TO X350J000-ID-ERROR-MOD

                 PERFORM 2400-ESCRIBIR-ERRORES
      *
                 MOVE SPACES             TO WS-RAMO-ORI
                                            WS-MODALIDAD-ORIGEN
      *
              ELSE
                  MOVE X880M007-KRAMO-S(1)  TO WS-RAMO-ORI
                  MOVE X350JPX3-MODALI-ORIGEN  TO WS-MODALIDAD-ORIGEN
      *
              END-IF
           .
      ******************************************************************44842002
      * 2300-OBTENER-NEW-GAP                                           *44843002
      ******************************************************************44844002
       2300-OBTENER-NEW-GAP.                                            44846002

      *D*
      *    DISPLAY '2300-OBTENER-NEW-GAP'
      *F*

           INITIALIZE WS-X340JIE0

           MOVE '01'                TO X340JIE0-CMODO-E                 44850012
           MOVE 'N'                 TO X340JIE0-CDOSGAP-E               44850113
           MOVE '000'               TO X340JIE0-KCOMPANIA-E(1:3)        50212
           MOVE X350JPX3-CCOMPANIA  TO X340JIE0-KCOMPANIA-E(4:2)        50312
      *    MOVE WS-FECHA            TO X340JIE0-FCIERRE-E               50412
PBTEC *    MOVE '20201231'          TO X340JIE0-FCIERRE-E               50412
PBTEC      MOVE R350T000-FFINAL     TO X340JIE0-FCIERRE-E               50412
           MOVE 'C'                 TO X340JIE0-TMODAL-E                50512
           MOVE 'N'                 TO X340JIE0-CLINKED-E               50612
           MOVE X350JPX3-CASADA     TO X340JIE0-CCASADA-E               50805

           IF  X350JPX3-CASADA EQUAL 'N'                                50907
              MOVE SPACES           TO X340JIE0-CGRUPOESPECIAL-E        51007

           END-IF                                                       51107

           MOVE '00'                TO X340JIE0-CPREGRUPO-E(1:2)        51205
           MOVE X350JPX3-CARTINV    TO X340JIE0-CPREGRUPO-E(3:2)        51305
           MOVE X350JPX3-FORIGEN-SUSCRI TO X340JIE0-FSUSCRIPCION-E      51405
           MOVE X350JPX3-PINTER-TECBASE     TO X340JIE0-ITECNICO-E      51505
                                                                        44851602
           CALL B340JIE0 USING WS-X340JIE0                              44851705
                                                                        44851802

           IF X340JIE0-CRETORNO-S EQUAL '00000'                         44851905
      *D*
      *    DISPLAY 'OK GAP'
      *    DISPLAY 'X340JIE0-GAP-S(1) ' X340JIE0-GAP-S(1)
      *F*
              MOVE X340JIE0-GAP-S(1) TO RSAL-GAP                        44852005
      *                                 X350JPX6-GAP
      *D*
      *    DISPLAY 'RSAL-GAP     ' RSAL-GAP
      *    DISPLAY 'X350JPX6-GAP ' X350JPX6-GAP                         44852617
      *F*
           ELSE                                                         44852717
      *D*
      *    DISPLAY 'NOK GAP'
      *F*
              MOVE '0099' TO RSAL-GAP                                   44852917
      *                      X350JPX6-GAP
      *D*
      *    DISPLAY 'RSAL-GAP     ' RSAL-GAP
      *    DISPLAY 'X350JPX6-GAP ' X350JPX6-GAP
      *F*
           END-IF                                                       44853617
                                                                        44856715
              .                                                         44859000

      ******************************************************************44842002
      * 2300-OBTENER-NEW-GAP-MOV                                       *44843002
      ******************************************************************44844002
       2300-OBTENER-NEW-GAP-MOV.                                        44846002

      *D*
      *    DISPLAY '2300-OBTENER-NEW-GAP-MOV'
      *F*

           INITIALIZE WS-X340JIE0

           MOVE '01'                TO X340JIE0-CMODO-E                 44850012
           MOVE 'N'                 TO X340JIE0-CDOSGAP-E               44850113
           MOVE '000'               TO X340JIE0-KCOMPANIA-E(1:3)        50212
           MOVE X350JXX2-CCOMPANIA  TO X340JIE0-KCOMPANIA-E(4:2)        50312
      *    MOVE WS-FECHA            TO X340JIE0-FCIERRE-E               50412
PBTEC *    MOVE '20201231'          TO X340JIE0-FCIERRE-E               50412
PBTEC      MOVE R350T000-FFINAL     TO X340JIE0-FCIERRE-E               50412
           MOVE 'C'                 TO X340JIE0-TMODAL-E                50512
           MOVE 'N'                 TO X340JIE0-CLINKED-E               50612
           MOVE X350JXX2-CASADA     TO X340JIE0-CCASADA-E               50805

           IF  X350JXX2-CASADA EQUAL 'N'                                50907
              MOVE SPACES           TO X340JIE0-CGRUPOESPECIAL-E        51007

           END-IF                                                       51107

           MOVE '00'                TO X340JIE0-CPREGRUPO-E(1:2)        51205
           MOVE X350JXX2-CARTINV    TO X340JIE0-CPREGRUPO-E(3:2)        51305
           MOVE X350JXX2-FORIGEN-SUSCRI TO X340JIE0-FSUSCRIPCION-E      51405
           MOVE X350JXX2-PINTER-TECBASE     TO X340JIE0-ITECNICO-E      51505
                                                                        44851602
           CALL B340JIE0 USING WS-X340JIE0                              44851705
                                                                        44851802

           IF X340JIE0-CRETORNO-S EQUAL '00000'                         44851905
      *D*
      *    DISPLAY 'OK GAP'
      *    DISPLAY 'X340JIE0-GAP-S(1) ' X340JIE0-GAP-S(1)
      *F*
              MOVE X340JIE0-GAP-S(1) TO RSAL-GAP                        44852005
      *                                 X350JPX6-GAP
      *D*
      *    DISPLAY 'RSAL-GAP     ' RSAL-GAP
      *    DISPLAY 'X350JPX6-GAP ' X350JPX6-GAP                         44852617
      *F*
           ELSE                                                         44852717
      *D*
      *    DISPLAY 'NOK GAP'
      *F*
              MOVE '0099' TO RSAL-GAP                                   44852917
      *                      X350JPX6-GAP
      *D*
      *    DISPLAY 'RSAL-GAP     ' RSAL-GAP
      *    DISPLAY 'X350JPX6-GAP ' X350JPX6-GAP
      *F*
           END-IF                                                       44853617
                                                                        44856715
              .                                                         44859000

      ******************************************************************10650099
       2400-ACUMULAR-MOVS.                                              10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2400-ACUMULAR-MOVS'
      *F*

MF001      IF R350T077-KCONCEPTO = '0206'
MF001         PERFORM REPARTIR-SINIESTRO
MF001      END-IF

           INITIALIZE RSALIDA
LUNA       MOVE X350JXX2-FECSIN-DESDE TO
LUNA                                    WS-FECSIN-DESDE
LUNA       MOVE X350JXX2-FECSIN-HASTA TO
LUNA                                    WS-FECSIN-HASTA


           IF R350T077-IMPORTE IS NUMERIC

              EVALUATE R350T077-KCONCEPTO
      *           WHEN ' 206'
      *                 IF R350T077-FFINAL > X350JXX2-FRENO-DESDE
      *                    ADD R350T077-IMPORTE TO WS-IMPORTE-206
      *                 ELSE
      *                    ADD R350T077-IMPORTE TO WS-IMPORTE-206-NR
      *                 END-IF

                  WHEN '0902'
                           ADD R350T077-IMPORTE TO WS-IMPORTE-902
              END-EVALUATE
           ELSE
              EVALUATE R350T077-KCONCEPTO
                  WHEN '0205'
      *D*
      *       DISPLAY 'R350T077-PBPAGADA ' R350T077-PBPAGADA
      *F*
                        ADD R350T077-IMPORTE-D TO WS-IMPORTE-205
RF003                     IF  SW-SI-LTBA
                              MOVE WS-FECHA-DESDE-TRAMO-ANT TO
                              WS-FRENO-DESDE-BONOA-PBPAG
                              MOVE WS-FECHA-HASTA-TRAMO-ANT TO
                              WS-FRENO-HASTA-BONOA-PBPAG

RF003                         COMPUTE WS-FRENO-DESDE-BONOA-PBPAG =
RF003                                 WS-FRENO-DESDE-BONOA-PBPAG + 10000
RF003                         COMPUTE WS-FRENO-HASTA-BONOA-PBPAG =
RF003                                 WS-FRENO-HASTA-BONOA-PBPAG + 10000
RF003                     END-IF
RF003                     IF SW-SI-LTB
                              MOVE WS-FRENO-DESDE-BONO-ACTUAL TO
                                   WS-FRENO-DESDE-BONO-PBPAG
                              MOVE WS-FRENO-HASTA-BONO-ACTUAL TO
                                   WS-FRENO-HASTA-BONO-PBPAG
RF003                         COMPUTE WS-FRENO-DESDE-BONO-PBPAG =
RF003                                 WS-FRENO-DESDE-BONO-PBPAG  + 10000
RF003                         COMPUTE WS-FRENO-HASTA-BONO-PBPAG =
RF003                                 WS-FRENO-HASTA-BONO-PBPAG  + 10000
RF003                     END-IF
RF003               IF ( SW-SI-LTBA
RF003               AND R350T077-FCONTABLE >= WS-FRENO-DESDE-BONOA-PBPAG
RF003              AND R350T077-FCONTABLE <  WS-FRENO-HASTA-BONOA-PBPAG)
RF003                    ADD R350T077-PBPAGADA  TO
RF003                                          WS-IMPORTE-PBP-BONO-FASE2
RF003               END-IF
RF003               IF ( SW-SI-LTB
RF003               AND R350T077-FCONTABLE >= WS-FRENO-DESDE-BONO-PBPAG
RF003              AND R350T077-FCONTABLE <  WS-FRENO-HASTA-BONO-PBPAG)
RF003                    ADD R350T077-PBPAGADA  TO WS-IMPORTE-PBP
RF003               END-IF
      *D*
      *       DISPLAY 'WS-IMPORTE-205     ' WS-IMPORTE-205
      *       DISPLAY 'R350T077-FEFECTO   ' R350T077-FEFECTO
      *       DISPLAY 'WS-IMPORTE-PBP     ' WS-IMPORTE-PBP
      *       DISPLAY 'PBP-BONO-FASE2     ' WS-IMPORTE-PBP-BONO-FASE2
      *       DISPLAY 'DESDE-BONOA-PBPAG  ' WS-FRENO-DESDE-BONOA-PBPAG
      *       DISPLAY 'DESDE-BONOA-PBPAG  ' WS-FRENO-HASTA-BONOA-PBPAG
      *       DISPLAY 'DESDE-BONO-PBPAG   ' WS-FRENO-DESDE-BONO-PBPAG
      *       DISPLAY 'DESDE-BONO-PBPAG   ' WS-FRENO-HASTA-BONO-PBPAG
      *       DISPLAY 'R350T077-GRESPONSABLE ' R350T077-GRESPONSABLE
      *F*

                  WHEN '0206'
      *           DISPLAY ' JOSE-PRUE-SINI   '
      *           DISPLAY ' X350JXX2-KPOLIZA ' X350JXX2-KPOLIZA
      *           DISPLAY ' X350JXX2-FRENO-DESDE ' X350JXX2-FRENO-DESDE
      *           DISPLAY ' X350JXX2-FRENO-HASTA ' X350JXX2-FRENO-HASTA
      *           DISPLAY ' R350T077-IMPORTE-D ' R350T077-IMPORTE-D
      *           DISPLAY ' R350T077-FEFECTO  ' R350T077-FEFECTO
      *           DISPLAY ' R350T077-FCONTABLE ' R350T077-FCONTABLE
      *           DISPLAY ' R350T077-KMOVIMIENTO ' R350T077-KMOVIMIENTO
      *                 IF R350T077-FFINAL > X350JXX2-FRENO-DESDE
      *                 IF R350T077-FEFECTO < X350JXX2-FRENO-HASTA

      *D*
      *       DISPLAY 'ES-RENOVADA: ' WS-RENOVADA
      *       DISPLAY 'HASTA-TRAMO-ANT: ' WS-FECHA-HASTA-TRAMO-ANT
      *F*

LUNN                  IF WS-RENOVADA = 'N' AND WS-ANTPBAN = 'S'
                                            AND RSAL-LTA = 'PB'
LUNN                    MOVE WS-FECSIN-DESDE-PB TO WS-FECSIN-DESDE
LUNN                  END-IF


290524          IF X350JXX2-FRENO-HASTA   >  R350T000-FFINAL
290524             MOVE 'S'                   TO WS-RENOVADA-SINI
290524          ELSE
290524             MOVE 'N'                   TO WS-RENOVADA-SINI
290524          END-IF


           IF  SW-NO-LTP AND SW-SI-LTPA

      *D*
      *       DISPLAY 'ENTRO ELSE NUEVO-ACU '
      *       DISPLAY 'R350T077-FCONTABLE   ' R350T077-FCONTABLE
      *       DISPLAY 'HASTA-TRAMO-ANT ' WS-FECHA-HASTA-TRAMO-ANT
      *       DISPLAY 'X350JXX2-FRENO-HASTA ' X350JXX2-FRENO-HASTA
      *F*
RF001                 IF R350T077-FEFECTO   >= WS-FECHA-HASTA-TRAMO-ANT

RF001                    IF R350T077-FEFECTO   <  X350JXX2-FRENO-HASTA
RF001                     ADD R350T077-IMPORTE-D TO WS-IMPORTE-206
RF001                    ELSE
RF001                      ADD R350T077-IMPORTE-D TO WS-IMPORTE-206-NR
RF001                    COMPUTE WS-FECSIN-AHASTA = WS-FECSIN-AHASTA + 1
RF001                    END-IF

      *D*
      *       DISPLAY 'WS-IMPORTE-206   : ' WS-IMPORTE-206
      *       DISPLAY 'WS-IMPORTE-206-NR : ' WS-IMPORTE-206-NR
      *F*
RF001                 END-IF

           ELSE


290524       IF WS-RENOVADA-SINI = 'N'
280624       IF  ( R350T077-CESTADORE = '01' OR
280624             R350T077-CESTADORE = '02' OR
280624             R350T077-CESTADORE = '04' OR
280624             R350T077-CESTADORE  = '05') OR
181224            (R350T077-CESTADORE = '20' AND
181224             (R350T077-FEFECTO   >= WS-FECSIN-DESDE  OR
181224             (R350T077-FEFECTO   <  WS-FECSIN-DESDE AND
                    R350T077-FCARGA > WS-FECSIN-DESDE)))

290524                IF R350T077-FEFECTO < X350JXX2-FRENO-HASTA
240624*                  R350T077-FEFECTO >= X350JXX2-FRENO-DESDE
290524                    ADD R350T077-IMPORTE-D TO WS-IMPORTE-206
290524                ELSE
290524                     ADD R350T077-IMPORTE-D TO WS-IMPORTE-206-NR
290524                   COMPUTE WS-FECSIN-AHASTA = WS-FECSIN-AHASTA + 1
290524                END-IF
280624       END-IF
290524       ELSE

280624       IF  ( R350T077-CESTADORE = '01' OR
280624             R350T077-CESTADORE = '02' OR
280624             R350T077-CESTADORE = '04' OR
280624             R350T077-CESTADORE = '05') OR
181224            (R350T077-CESTADORE = '20' AND
181224             (R350T077-FEFECTO   >= WS-FECSIN-DESDE  OR
181224             (R350T077-FEFECTO   <  WS-FECSIN-DESDE AND
181224              R350T077-FCARGA > WS-FECSIN-DESDE)))

      *               IF R350T077-FEFECTO < WS-FECSIN-HASTA AND
260624*               IF R350T077-FEFECTO >= X350JXX2-FRENO-DESDE AND
260624                IF R350T077-FEFECTO <  X350JXX2-FRENO-HASTA

220624*               IF R350T077-FEFECTO >= WS-FECSIN-DESDE AND
240624*                  R350T077-FEFECTO <  WS-FECSIN-HASTA
                          ADD R350T077-IMPORTE-D TO WS-IMPORTE-206
                      ELSE
                        ADD R350T077-IMPORTE-D TO WS-IMPORTE-206-NR
LUNA  *                 COMPUTE WS-FECSIN-AHASTA = WS-FECSIN-AHASTA + 1
                      END-IF
280624       END-IF
290524       END-IF
RF001      END-IF
110624*             IF SW-SI-LTB AND SW-NO-LTP
110624*                MOVE ZEROES TO WS-IMPORTE-206
110624*                MOVE ZEROES TO WS-IMPORTE-206-NR
110624*             END-IF

                  WHEN '0902'
REVI                    ADD R350T077-IMPORTE-D TO WS-IMPORTE-902
RF003                     IF  SW-SI-LTBA
                              MOVE WS-FECHA-DESDE-TRAMO-ANT TO
                              WS-FRENO-DESDE-BONOA-PBPAG
                              MOVE WS-FECHA-HASTA-TRAMO-ANT TO
                              WS-FRENO-HASTA-BONOA-PBPAG

RF003                         COMPUTE WS-FRENO-DESDE-BONOA-PBPAG =
RF003                                 WS-FRENO-DESDE-BONOA-PBPAG + 10000
RF003                         COMPUTE WS-FRENO-HASTA-BONOA-PBPAG =
RF003                                 WS-FRENO-HASTA-BONOA-PBPAG + 10000
RF003                     END-IF
RF003                     IF SW-SI-LTB
                              MOVE WS-FRENO-DESDE-BONO-ACTUAL TO
                                   WS-FRENO-DESDE-BONO-PBPAG
                              MOVE WS-FRENO-HASTA-BONO-ACTUAL TO
                                   WS-FRENO-HASTA-BONO-PBPAG
RF003                         COMPUTE WS-FRENO-DESDE-BONO-PBPAG =
RF003                                 WS-FRENO-DESDE-BONO-PBPAG  + 10000
RF003                         COMPUTE WS-FRENO-HASTA-BONO-PBPAG =
RF003                                 WS-FRENO-HASTA-BONO-PBPAG  + 10000
RF003                     END-IF
RF003               IF ( SW-SI-LTBA
RF003               AND R350T077-FCONTABLE >= WS-FRENO-DESDE-BONOA-PBPAG
RF003              AND R350T077-FCONTABLE <  WS-FRENO-HASTA-BONOA-PBPAG)
RF003                    ADD R350T077-PBPAGADA  TO
RF003                                          WS-IMPORTE-PBP-BONO-FASE2
RF003               END-IF
RF003               IF ( SW-SI-LTB
RF003               AND R350T077-FCONTABLE >= WS-FRENO-DESDE-BONO-PBPAG
RF003              AND R350T077-FCONTABLE <  WS-FRENO-HASTA-BONO-PBPAG)
RF003                    ADD R350T077-PBPAGADA  TO WS-IMPORTE-PBP
RF003               END-IF
      *D*
      *       DISPLAY 'WS-IMPORTE-205     ' WS-IMPORTE-205
      *       DISPLAY 'R350T077-FEFECTO   ' R350T077-FEFECTO
      *       DISPLAY 'WS-IMPORTE-PBP     ' WS-IMPORTE-PBP
      *       DISPLAY 'PBP-BONO-FASE2     ' WS-IMPORTE-PBP-BONO-FASE2
      *       DISPLAY 'DESDE-BONOA-PBPAG  ' WS-FRENO-DESDE-BONOA-PBPAG
      *       DISPLAY 'DESDE-BONOA-PBPAG  ' WS-FRENO-HASTA-BONOA-PBPAG
      *       DISPLAY 'DESDE-BONO-PBPAG   ' WS-FRENO-DESDE-BONO-PBPAG
      *       DISPLAY 'DESDE-BONO-PBPAG   ' WS-FRENO-HASTA-BONO-PBPAG
      *       DISPLAY 'R350T077-GRESPONSABLE ' R350T077-GRESPONSABLE
      *F*
              END-EVALUATE
           END-IF

PBTEC      MOVE WS-POLIZA-1                TO WS-POLIZA-1-ANT
           MOVE WS-SUBPOL-1                TO WS-SUBPOL-1-ANT

           PERFORM 3100-LEER-ENTRADA-MOVS

           .

      ******************************************************************10650099
       2400-ACUMULAR-MOVS-LTP.                                          10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2400-ACUMULAR-MOVS-LTP'
      *    DISPLAY 'X350JXX2-KPOLIZA      ' X350JXX2-KPOLIZA
      *    DISPLAY 'X350JXX2-KSUBPOL      ' X350JXX2-KSUBPOL
      *    DISPLAY 'R350T077-KMOVIMIENTO  ' R350T077-KMOVIMIENTO
      *    DISPLAY 'R350T077-FCONTABLE    ' R350T077-FCONTABLE
      *    DISPLAY 'R350T077-FEFECTO      ' R350T077-FEFECTO
      *    DISPLAY 'X350JXX2-FRENO-DESDE  ' X350JXX2-FRENO-DESDE
      *    DISPLAY 'X350JXX2-FRENO-HASTA  ' X350JXX2-FRENO-HASTA
      *    DISPLAY 'WS-FECHA-DESDE-TRAMO  ' WS-FECHA-DESDE-TRAMO
      *    DISPLAY 'WS-FECHA-HASTA-TRAMO  ' WS-FECHA-HASTA-TRAMO
      *    DISPLAY 'R350T000-FFINAL       ' R350T000-FFINAL
      *F*
MF001      IF R350T077-KCONCEPTO = '0206'
MF001         PERFORM REPARTIR-SINIESTRO
MF001      END-IF

151124          IF X350JXX2-FRENO-HASTA   >  R350T000-FFINAL
151124             MOVE 'S'                   TO WS-RENOVADA-ALFA
151124          ELSE
151124             MOVE 'N'                   TO WS-RENOVADA-ALFA
151124          END-IF

151124          IF WS-RENOVADA-ALFA = 'S'
151124             MOVE X350JXX2-FRENO-DESDE TO WS-FECHA-ALFA
151124          ELSE
151124             MOVE X350JXX2-FRENO-DESDE TO WS-FECHA-ALFA
151124             ADD 1 TO WS-ANNO-FECHA-ALFA
151124          END-IF

0124       IF SW-SI-LTA-FINAL AND SW-SW-LTPA EQUAL 'N'
0124                          AND WS-RENOVADA-LTA EQUAL 'N'
0124
0124          MOVE X350JXX2-FECSIN-DESDE TO
0124                                       WS-FECSIN-DESDE
0124          MOVE X350JXX2-FECSIN-HASTA TO
0124                                       WS-FECSIN-HASTA
           ELSE

PLURIN        MOVE WS-FRENO-DESDE-PLURI-ACTUAL TO WS-FECSIN-DESDE
LUNA          MOVE WS-FECHA-HASTA-TRAMO        TO WS-FECSIN-HASTA
LUNA
0124       END-IF

           IF R350T077-IMPORTE IS NUMERIC

              EVALUATE R350T077-KCONCEPTO
      *           WHEN '0206'
      *                 IF R350T077-FFINAL > X350JXX2-FRENO-DESDE
      *                    ADD R350T077-IMPORTE TO WS-IMPORTE-206
      *                 ELSE
      *                    ADD R350T077-IMPORTE TO WS-IMPORTE-206-NR
      *                 END-IF

                  WHEN '0902'
                           ADD R350T077-IMPORTE TO WS-IMPORTE-902
                           ADD R350T077-IMPORTE-D TO WS-IMP-PERI-CAL3
              END-EVALUATE
           ELSE
              EVALUATE R350T077-KCONCEPTO
                  WHEN '0205'
      *D*
      *       DISPLAY '-------------- 205 ----------------'
      *       DISPLAY 'R350T077-KPOLIZA     ' R350T077-KPOLIZA
      *       DISPLAY 'R350T077-KSUBPOLIZA  ' R350T077-KSUBPOLIZA
      *       DISPLAY 'R350T077-FCONTABLE   ' R350T077-FCONTABLE
      *       DISPLAY 'X350JXX2-FRENO-DESDE ' X350JXX2-FRENO-DESDE
      *       DISPLAY 'X350JXX2-FRENO-HASTA ' X350JXX2-FRENO-HASTA
      *F*
110624*               IF R350T077-FCONTABLE >= WS-FECHA-HASTA-TRAMO
110624                IF R350T077-FCONTABLE >= WS-FECHA-ALFA
240524*               IF R350T077-FCONTABLE >= X350JXX2-FRENO-HASTA
JOSE  *              AND R350T077-FCONTABLE <  X350JXX2-FRENO-HASTA
JOSE  *               IF R350T077-FCONTABLE >= WS-ANNO-DESDE-TRAMO
JOSE  *              AND R350T077-FCONTABLE <  WS-ANNO-HASTA-TRAMO

      *D*
      *       DISPLAY 'R350T077-IMPORTE-D   ' R350T077-IMPORTE-D
      *F*
                       ADD R350T077-IMPORTE-D TO WS-IMPORTE-205
RF003                     IF  SW-SI-LTBA
                              MOVE WS-FECHA-DESDE-TRAMO-ANT TO
                              WS-FRENO-DESDE-BONOA-PBPAG
                              MOVE WS-FECHA-HASTA-TRAMO-ANT TO
                              WS-FRENO-HASTA-BONOA-PBPAG

RF003                         COMPUTE WS-FRENO-DESDE-BONOA-PBPAG =
RF003                                 WS-FRENO-DESDE-BONOA-PBPAG + 10000
RF003                         COMPUTE WS-FRENO-HASTA-BONOA-PBPAG =
RF003                                 WS-FRENO-HASTA-BONOA-PBPAG + 10000
RF003                     END-IF
RF003                     IF SW-SI-LTB
                              MOVE WS-FRENO-DESDE-BONO-ACTUAL TO
                                   WS-FRENO-DESDE-BONO-PBPAG
                              MOVE WS-FRENO-HASTA-BONO-ACTUAL TO
                                   WS-FRENO-HASTA-BONO-PBPAG
RF003                         COMPUTE WS-FRENO-DESDE-BONO-PBPAG =
RF003                                 WS-FRENO-DESDE-BONO-PBPAG  + 10000
RF003                         COMPUTE WS-FRENO-HASTA-BONO-PBPAG =
RF003                                 WS-FRENO-HASTA-BONO-PBPAG  + 10000
RF003                     END-IF
RF003               IF ( SW-SI-LTBA
RF003               AND R350T077-FCONTABLE >= WS-FRENO-DESDE-BONOA-PBPAG
RF003              AND R350T077-FCONTABLE <  WS-FRENO-HASTA-BONOA-PBPAG)
RF003                    ADD R350T077-PBPAGADA  TO
RF003                                          WS-IMPORTE-PBP-BONO-FASE2
RF003               END-IF
RF003               IF ( SW-SI-LTB
RF003               AND R350T077-FCONTABLE >= WS-FRENO-DESDE-BONO-PBPAG
RF003              AND R350T077-FCONTABLE  <  WS-FRENO-HASTA-BONO-PBPAG)
RF003                    ADD R350T077-PBPAGADA  TO WS-IMPORTE-PBP-BONO
RF003               ELSE
RF003                    MOVE ZEROES       TO WS-IMPORTE-PBP
RF003               END-IF
      *D*
      *       DISPLAY 'WS-IMPORTE-205     ' WS-IMPORTE-205
      *       DISPLAY 'R350T077-FCONTABLE ' R350T077-FCONTABLE
      *       DISPLAY 'WS-IMPORTE-PBP     ' WS-IMPORTE-PBP
      *       DISPLAY 'WS-IMPORTE-PBP-BONO ' WS-IMPORTE-PBP-BONO
      *       DISPLAY 'PBP-BONO-FASE2     ' WS-IMPORTE-PBP-BONO-FASE2
      *       DISPLAY 'DESDE-BONOA-PBPAG  ' WS-FRENO-DESDE-BONOA-PBPAG
      *       DISPLAY 'DESDE-BONOA-PBPAG  ' WS-FRENO-HASTA-BONOA-PBPAG
      *       DISPLAY 'DESDE-BONO-PBPAG   ' WS-FRENO-HASTA-BONOA-PBPAG
      *       DISPLAY 'DESDE-BONO-PBPAG   ' WS-FRENO-HASTA-BONOA-PBPAG
      *       DISPLAY 'R350T077-KMOVIMIENTO ' R350T077-KMOVIMIENTO
      *F*
      *               MOVE ZEROES             TO WS-IMPORTE-PBP
PBTEC *                ADD R350T077-PBPAGADA  TO WS-IMPORTE-PBP
                       ADD R350T077-IMPORTE-D TO WS-IMP-PERI-CAL3
      *D*
      *       DISPLAY 'WS-IMP-PERI-CAL3     ' WS-IMP-PERI-CAL3
      *F*
                      ELSE
      *                 IF R350T077-FCONTABLE < X350JXX2-FRENO-DESDE
      *                 IF R350T077-FCONTABLE < WS-ANNO-DESDE-TRAMO
      *D*
      *       DISPLAY 'R350T077-IMPORTE-D   ' R350T077-IMPORTE-D
      *F*
                          ADD R350T077-IMPORTE-D TO WS-IMPORTE-205
RF003                     IF  SW-SI-LTBA
                              MOVE WS-FECHA-DESDE-TRAMO-ANT TO
                              WS-FRENO-DESDE-BONOA-PBPAG
                              MOVE WS-FECHA-HASTA-TRAMO-ANT TO
                              WS-FRENO-HASTA-BONOA-PBPAG

RF003                         COMPUTE WS-FRENO-DESDE-BONOA-PBPAG =
RF003                                 WS-FRENO-DESDE-BONOA-PBPAG + 10000
RF003                         COMPUTE WS-FRENO-HASTA-BONOA-PBPAG =
RF003                                 WS-FRENO-HASTA-BONOA-PBPAG + 10000
RF003                     END-IF
RF003                     IF SW-SI-LTB
                              MOVE WS-FRENO-DESDE-BONO-ACTUAL TO
                                   WS-FRENO-DESDE-BONO-PBPAG
                              MOVE WS-FRENO-HASTA-BONO-ACTUAL TO
                                   WS-FRENO-HASTA-BONO-PBPAG
RF003                         COMPUTE WS-FRENO-DESDE-BONO-PBPAG =
RF003                                 WS-FRENO-DESDE-BONO-PBPAG  + 10000
RF003                         COMPUTE WS-FRENO-HASTA-BONO-PBPAG =
RF003                                 WS-FRENO-HASTA-BONO-PBPAG  + 10000
RF003                     END-IF
RF003               IF ( SW-SI-LTBA
RF003               AND R350T077-FCONTABLE >= WS-FRENO-DESDE-BONOA-PBPAG
RF003              AND R350T077-FCONTABLE <  WS-FRENO-HASTA-BONOA-PBPAG)
RF003                    ADD R350T077-PBPAGADA  TO
RF003                                          WS-IMPORTE-PBP-BONO-FASE2
RF003               END-IF
RF003               IF ( SW-SI-LTB
RF003               AND R350T077-FCONTABLE >= WS-FRENO-DESDE-BONO-PBPAG
RF003              AND R350T077-FCONTABLE  <  WS-FRENO-HASTA-BONO-PBPAG)
RF003                    ADD R350T077-PBPAGADA  TO WS-IMPORTE-PBP-BONO
RF003               ELSE
RF003                    MOVE ZEROES       TO WS-IMPORTE-PBP
RF003               END-IF
      *D*
      *       DISPLAY 'WS-IMPORTE-205     ' WS-IMPORTE-205
      *       DISPLAY 'R350T077-FCONTABLE ' R350T077-FCONTABLE
      *       DISPLAY 'WS-IMPORTE-PBP     ' WS-IMPORTE-PBP
      *       DISPLAY 'WS-IMPORTE-PBP-BONO ' WS-IMPORTE-PBP-BONO
      *       DISPLAY 'PBP-BONO-FASE2     ' WS-IMPORTE-PBP-BONO-FASE2
      *       DISPLAY 'DESDE-BONOA-PBPAG  ' WS-FRENO-DESDE-BONOA-PBPAG
      *       DISPLAY 'DESDE-BONOA-PBPAG  ' WS-FRENO-HASTA-BONOA-PBPAG
      *       DISPLAY 'DESDE-BONO-PBPAG   ' WS-FRENO-HASTA-BONOA-PBPAG
      *       DISPLAY 'DESDE-BONO-PBPAG   ' WS-FRENO-HASTA-BONOA-PBPAG
      *       DISPLAY 'R350T077-KMOVIMIENTO ' R350T077-KMOVIMIENTO
      *F*
                          ADD R350T077-IMPORTE-D TO WS-IMP-PERI-CAL2
      *D*
      *       DISPLAY 'WS-IMP-PERI-CAL2     ' WS-IMP-PERI-CAL2
      *F*
                        END-IF

JOSE  *                 IF R350T077-FCONTABLE >= X350JXX2-FRENO-HASTA
JOSE  *                AND R350T077-FCONTABLE <= R350T000-FFINAL
JOSE  *                 IF R350T077-FEFECTO   >  R350T000-FFINAL
JOSE  *                AND R350T077-FCONTABLE <= R350T000-FFINAL
      *D*
      *       DISPLAY 'R350T077-IMPORTE-D   ' R350T077-IMPORTE-D
      *F*
      *                   ADD R350T077-IMPORTE-D TO WS-IMPORTE-205
PBTEC *                   ADD R350T077-PBPAGADA  TO WS-IMPORTE-PBP
PBTEC *                   MOVE ZEROES            TO WS-IMPORTE-PBP
      *                   ADD R350T077-IMPORTE-D TO WS-IMP-PERI-CAL3
      *D*
      *       DISPLAY 'WS-IMP-PERI-CAL3     ' WS-IMP-PERI-CAL3
      *F*
      *                 END-IF
      *               END-IF

                  WHEN '0206'
      *           DISPLAY ' JOSE-PRUE-SINI   '
      *           DISPLAY ' X350JXX2-KPOLIZA ' X350JXX2-KPOLIZA
      *           DISPLAY ' X350JXX2-SUBPOL  ' X350JXX2-KSUBPOL
      *           DISPLAY ' X350JXX2-FRENO-DESDE ' X350JXX2-FRENO-DESDE
      *           DISPLAY ' X350JXX2-FRENO-HASTA ' X350JXX2-FRENO-HASTA
      *           DISPLAY ' R350T077-IMPORTE-D ' R350T077-IMPORTE-D
      *           DISPLAY ' R350T077-FEFECTO  ' R350T077-FEFECTO
      *           DISPLAY ' R350T077-FCONTABLE ' R350T077-FCONTABLE
      *           DISPLAY ' R350T077-KMOVIMIENTO ' R350T077-KMOVIMIENTO
      *                 IF R350T077-FFINAL > X350JXX2-FRENO-DESDE
      *                 IF R350T077-FEFECTO < X350JXX2-FRENO-HASTA
      *D*
      *       DISPLAY 'ES-RENOVADA: ' WS-RENOVADA
      *       DISPLAY 'HASTA-TRAMO    : ' WS-FECHA-HASTA-TRAMO
      *F*


LUNN  *               IF WS-RENOVADA = 'N' AND WS-ANTPBAN = 'S'
LUNN  *                 MOVE WS-FECSIN-DESDE-PB TO WS-FECSIN-DESDE
      *               ELSE
      *                 MOVE WS-FECHA-DESDE-TRAMO TO WS-FECSIN-DESDE
LUNN  *               END-IF

5-PRO                   IF R350T077-FCONTABLE < WS-FECHA-HASTA-TRAMO OR
                           R350T077-FEFECTO < WS-FECHA-HASTA-TRAMO
PLURIN*                    IF R350T077-FCONTABLE > WS-FECHA-DESDE-TRAMO
PLURIN*                    AND R350T077-FEFECTO  > WS-FECHA-DESDE-TRAMO
                        IF R350T077-FCONTABLE >
PLURIN                                     WS-FRENO-DESDE-PLURI-ACTUAL
                        AND R350T077-FEFECTO >
PLURIN                                     WS-FRENO-DESDE-PLURI-ACTUAL
                           AND SW-SI-LTPA
                             ADD R350T077-IMPORTE-D TO WS-IMPORTE-206
      *D*
      *       DISPLAY ' WS-IMPORTE-206-IF ' WS-IMPORTE-206
      *F*
                        ELSE
      *D*
      *       DISPLAY ' WS-IMPORTE-206-ELSE' WS-IMPORTE-206-SINI
      *F*
                          ADD R350T077-IMPORTE-D TO WS-IMPORTE-206-SINI

                        END-IF
      *D*
      *           DISPLAY ' R350T077-IMPORTE-D ' R350T077-IMPORTE-D
      *           DISPLAY ' WS-IMPORTE-206       ' WS-IMPORTE-206
      *           DISPLAY ' SUMA-IMPORTE         ' WS-IMPORTE-206
      *F*
                        ELSE
                          ADD R350T077-IMPORTE-D TO WS-IMPORTE-206-NR
      *D*
      *           DISPLAY ' SUMA-IMPORTE-NR      ' WS-IMPORTE-206-NR
      *           DISPLAY ' R350T077-IMPORTE-D ' R350T077-IMPORTE-D
      *           DISPLAY ' WS-IMPORTE-206-NR     ' WS-IMPORTE-206-NR
      *F*

                        END-IF
                  WHEN '0902'
                        ADD R350T077-IMPORTE-D TO WS-IMPORTE-902
                        ADD R350T077-IMPORTE-D TO WS-IMP-PERI-CAL3
RF003                     IF  SW-SI-LTBA
                              MOVE WS-FECHA-DESDE-TRAMO-ANT TO
                              WS-FRENO-DESDE-BONOA-PBPAG
                              MOVE WS-FECHA-HASTA-TRAMO-ANT TO
                              WS-FRENO-HASTA-BONOA-PBPAG

RF003                         COMPUTE WS-FRENO-DESDE-BONOA-PBPAG =
RF003                                 WS-FRENO-DESDE-BONOA-PBPAG + 10000
RF003                         COMPUTE WS-FRENO-HASTA-BONOA-PBPAG =
RF003                                 WS-FRENO-HASTA-BONOA-PBPAG + 10000
RF003                     END-IF
RF003                     IF SW-SI-LTB
                              MOVE WS-FRENO-DESDE-BONO-ACTUAL TO
                                   WS-FRENO-DESDE-BONO-PBPAG
                              MOVE WS-FRENO-HASTA-BONO-ACTUAL TO
                                   WS-FRENO-HASTA-BONO-PBPAG
RF003                         COMPUTE WS-FRENO-DESDE-BONO-PBPAG =
RF003                                 WS-FRENO-DESDE-BONO-PBPAG  + 10000
RF003                         COMPUTE WS-FRENO-HASTA-BONO-PBPAG =
RF003                                 WS-FRENO-HASTA-BONO-PBPAG  + 10000
RF003                     END-IF
RF003               IF ( SW-SI-LTBA
RF003               AND R350T077-FCONTABLE >= WS-FRENO-DESDE-BONOA-PBPAG
RF003              AND R350T077-FCONTABLE <  WS-FRENO-HASTA-BONOA-PBPAG)
RF003                    ADD R350T077-PBPAGADA  TO
RF003                                          WS-IMPORTE-PBP-BONO-FASE2
RF003               END-IF
RF003               IF ( SW-SI-LTB
RF003               AND R350T077-FCONTABLE >= WS-FRENO-DESDE-BONO-PBPAG
RF003              AND R350T077-FCONTABLE  <  WS-FRENO-HASTA-BONO-PBPAG)
RF003                    ADD R350T077-PBPAGADA  TO WS-IMPORTE-PBP-BONO
RF003               ELSE
RF003                    MOVE ZEROES       TO WS-IMPORTE-PBP
RF003               END-IF
      *D*
      *       DISPLAY 'WS-IMPORTE-205     ' WS-IMPORTE-205
      *       DISPLAY 'R350T077-FCONTABLE ' R350T077-FCONTABLE
      *       DISPLAY 'WS-IMPORTE-PBP     ' WS-IMPORTE-PBP
      *       DISPLAY 'WS-IMPORTE-PBP-BONO ' WS-IMPORTE-PBP-BONO
      *       DISPLAY 'PBP-BONO-FASE2     ' WS-IMPORTE-PBP-BONO-FASE2
      *       DISPLAY 'DESDE-BONOA-PBPAG  ' WS-FRENO-DESDE-BONOA-PBPAG
      *       DISPLAY 'DESDE-BONOA-PBPAG  ' WS-FRENO-HASTA-BONOA-PBPAG
      *       DISPLAY 'DESDE-BONO-PBPAG   ' WS-FRENO-HASTA-BONOA-PBPAG
      *       DISPLAY 'DESDE-BONO-PBPAG   ' WS-FRENO-HASTA-BONOA-PBPAG
      *       DISPLAY 'R350T077-KMOVIMIENTO ' R350T077-KMOVIMIENTO
      *F*
      *D*
      *       DISPLAY 'WS-IMP-PERI-CAL3-902 ' WS-IMP-PERI-CAL3
      *F*
              END-EVALUATE
           END-IF

PBTEC      MOVE WS-POLIZA-1                TO WS-POLIZA-1-ANT
           MOVE WS-SUBPOL-1                TO WS-SUBPOL-1-ANT

           PERFORM 3100-LEER-ENTRADA-MOVS

           .

      ******************************************************************10650099
       2500-TRATAR-SUSCRIP.                                             10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2500-TRATAR-SUSCRIP'
      *    DISPLAY 'ES RENOVADA? ' WS-RENOVADA
      *F*
           INITIALIZE X350JPX3

           MOVE X350JXX2                      TO X350JPX3

           COMPUTE WS-IMPORTE-SUS  = X350JPX3-EPNETA + WS-IMPORTE-SUS
           COMPUTE WS-IMPORTE-PRI  = X350JPX3-EPNETA + WS-IMPORTE-PRI

           IF WS-RENOVADA = 'S'
              MOVE 'S'                  TO RSAL-RENOVADA
                                           X350JPX6-RENOVADA
              PERFORM 2800-TRATAR-RENO

           ELSE
              MOVE 'N'                  TO RSAL-RENOVADA
                                           X350JPX6-RENOVADA

              INITIALIZE WS-CONT-REGS

              PERFORM 2900-TRATAR-NO-RENO
PRUJA           UNTIL WS-CONT-REGS = 2
           END-IF
      *


           PERFORM 3200-LEER-ENTRADA-CTEC


           IF WS-CLAVE2        NOT = WS-CLAVE2-ANT
              PERFORM 5100-GRABAR-SALIDA-SUSCRIP

LTA          IF SW-SI-LTB
      *D*
      *         DISPLAY 'TRATAR REGISTRO LTB'
      *F*
                PERFORM 5200-GESTION-LTA
LTA          END-IF

              INITIALIZE WS-IMPORTE-SUS
                         WS-IMPORTE-PRI
           END-IF
           .

      ******************************************************************10650099
       2600-LINEA-MVTOS.                                                10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2600-LINEA-MVTOS COMIENZO'
      *    DISPLAY 'X350JXX2-KPOLIZA ' X350JXX2-KPOLIZA
      *    DISPLAY 'X350JXX2-KSUBPOL ' X350JXX2-KSUBPOL
      *    DISPLAY 'X350JXX2-FRENO-DESDEMVTO ' X350JXX2-FRENO-DESDE
      *    DISPLAY 'X350JXX2-FRENO-HASTAMVTO ' X350JXX2-FRENO-HASTA
      *    DISPLAY 'FECHA DESDE PB: ' WS-FECSIN-DESDE-PB
      *    DISPLAY 'FECHA HASTA: ' WS-FECSIN-HASTA
      *F*

SINUSO*MF001 INITIALIZE DATOS-CTEC


                MOVE ZEROES TO X350JPX6-PRIMA-ANUAL

                MOVE '3000'                   TO RSAL-MODELO
                MOVE X350JXX2-FRENO-DESDE TO WS-FECHA-AUX-CNTL

                COMPUTE WS-ANIO-CNTL = WS-ANIO-CNTL + 1

      *D*
      *         DISPLAY 'WS-FECHA-AUX-CNTL ' WS-FECHA-AUX-CNTL
      *         DISPLAY 'X350JXX2-FRENO-HASTA ' X350JXX2-FRENO-AHASTA
      *F*
                IF WS-ANIO-CNTL      < X350JXX2-FRENO-AHASTA
                   MOVE WS-FECHA-AUX-CNTL TO X350JXX2-FRENO-HASTA
                END-IF
      *D*
      *         DISPLAY 'X350JXX2-FRENO-HASTA ' X350JXX2-FRENO-HASTA
      *         DISPLAY 'R350T000-FFINAL ' R350T000-FFINAL
      *F*
PBTEC           IF X350JXX2-FRENO-HASTA   >  R350T000-FFINAL
PBTEC *         OR X350JXX2-FRENO-HASTA   =  R350T000-FFINAL
                   MOVE 'S'                   TO RSAL-RENOVADA
                                                 X350JPX6-RENOVADA
                   MOVE 'S'                   TO WS-RENOVADA
LTA                MOVE 'PB'                  TO RSAL-LTA
                ELSE
      *D*
      *         DISPLAY 'NO RENO'
      *F*
                   MOVE 'N'                   TO RSAL-RENOVADA
                                                 X350JPX6-RENOVADA
                   MOVE 'N'                   TO WS-RENOVADA
LTA                MOVE 'PBAN'                TO RSAL-LTA
                   MOVE 'S'                   TO WS-ANTPBAN
                END-IF
      *D*
      *    DISPLAY '2600-LINEA-MVTOS RSAL-LTA ' RSAL-LTA
      *F*
      *

                PERFORM CALCULAR-ANNOS-TRANS-MVT

130225          IF RSAL-LTA = 'PBAN'
130225             MOVE 1 TO WS-ALFA
130225          ELSE
                   PERFORM 3301-RECUPERAR-ALFA-MVT
130225          END-IF

                MOVE X350JXX2-CCANAL          TO RSAL-CANAL
                                                 X350JPX6-CANAL
                MOVE X350JXX2-KPOLIZA         TO RSAL-POLIZA
                MOVE X350JXX2-KSUBPOL         TO RSAL-SUBPOLIZA
                MOVE SPACES                   TO RSAL-SUSCRIP
                MOVE X350JXX2-KRAMO           TO RSAL-RAMO
                                                 X350JPX6-RAMO
                MOVE X350JXX2-MODALI          TO RSAL-MODALIDAD
                                                 X350JPX6-MODALIDAD
                MOVE X350JXX2-VIGOR-ANUL      TO RSAL-CESTADOASEG
                MOVE '00'                     TO RSAL-PREGRUPO(1:2)
                MOVE X350JXX2-CARTINV         TO RSAL-PREGRUPO(3:2)
      *D*
      *         DISPLAY 'X350JXX2-FRENO-DESDE ' X350JXX2-FRENO-DESDE
      *         DISPLAY 'X350JXX2-FRENO-HASTA ' X350JXX2-FRENO-HASTA
      *F*

                MOVE X350JXX2-FRENO-ADESDE    TO RSAL-FRENO-ADESDE
                MOVE X350JXX2-FRENO-MDESDE    TO RSAL-FRENO-MDESDE
                MOVE X350JXX2-FRENO-DDESDE    TO RSAL-FRENO-DDESDE

                MOVE X350JXX2-FRENO-AHASTA TO RSAL-FRENO-AHASTA
                MOVE X350JXX2-FRENO-MHASTA TO RSAL-FRENO-MHASTA
                MOVE X350JXX2-FRENO-DHASTA TO RSAL-FRENO-DHASTA

                IF RSAL-RENOVADA = 'N'
                   MOVE X350JXX2-FRENO-AHASTA TO    RSAL-FCIERREA
                                                    X350JPX6-FCIERREA
                   MOVE X350JXX2-FRENO-MHASTA TO    RSAL-FCIERREM
                                                    X350JPX6-FCIERREM
                   MOVE X350JXX2-FRENO-DHASTA TO    RSAL-FCIERRED
                                                    X350JPX6-FCIERRED

                   MOVE X350JXX2-FRENO-AHASTA TO    WS-FECSIN-ADESDEPB
                   MOVE X350JXX2-FRENO-MHASTA TO    WS-FECSIN-MDESDEPB
                   MOVE X350JXX2-FRENO-DHASTA TO    WS-FECSIN-DDESDEPB
                ELSE
                   MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
                                                    X350JPX6-FCIERREA
                   MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
                                                    X350JPX6-FCIERREM
                   MOVE R350T000-FDFINAL         TO RSAL-FCIERRED
                                                    X350JPX6-FCIERRED
      *            MOVE WS-FECSIN-ADESDEPB    TO    WS-FECSIN-ADESDE
      *            MOVE WS-FECSIN-MDESDEPB    TO    WS-FECSIN-MDESDE
      *            MOVE WS-FECSIN-DDESDEPB    TO    WS-FECSIN-DDESDE

                END-IF
      *D*
      *         DISPLAY 'COMPRUEBA ANULADA ' X350JXX2-VIGOR-ANUL
      *F*
                IF X350JXX2-VIGOR-ANUL  = 'A'
                   MOVE X350JXX2-ANO-ANUL     TO RSAL-FECAANULACION
                   MOVE X350JXX2-MES-ANUL     TO RSAL-FECMANULACION
                   MOVE X350JXX2-DIA-ANUL     TO RSAL-FECDANULACION
                   MOVE X350JXX2-MOTIVO-ANU   TO RSAL-MOTIVO
                END-IF

                MOVE X350JXX2-FCOBRO          TO RSAL-FORMAPAGO
                MOVE X350JXX2-TIPO-COASE      TO RSAL-COASEGURO
                MOVE X350JXX2-PCOASE          TO RSAL-PCOASEGURO
                MOVE X350JXX2-PAGADOR         TO RSAL-PGLOBAL

      *D*
      *        DISPLAY 'COMPRUEBA MECANIZADA '
      *F*
               IF X350JXX2-CERTI EQUAL '000000' AND                     37430000
                  X350JXX2-ASEG-MECANIZADOS = 'N'                       37440000
                  MOVE 'S' TO RSAL-IMNOMINADA                           37490000

               ELSE
                  MOVE 'N' TO RSAL-IMNOMINADA                           37490000

               END-IF

                MOVE X350JXX2-NASEGURADO      TO RSAL-NASEGURADOS

                MOVE WS-ALFA                  TO RSAL-ALFA


200524          IF SW-SI-LTB
200524             MOVE X350M004-KGRUPEMP-BS(3:3) TO X350JXX2-KGRUEMP
200524          END-IF

200524          IF X350JXX2-KGRUEMP = ZEROS
200524            MOVE SPACES           TO X350JXX2-KGRUEMP
200524          END-IF
                MOVE X350JXX2-KGRUEMP         TO RSAL-GEMPRESA


PBTECJ*    IF AUX-TIP-REAJUSTE = '1' OR 'T'
      *       MOVE 'S'                   TO RSAL-REAJUSTE
      *    ELSE
      *       MOVE 'N'                   TO RSAL-REAJUSTE
      *    END-IF

                MOVE AUX-CCOBON-REAJUS        TO RSAL-REAJUSTE

                IF X350X098-IANTICIPO-S > ZEROES
                   MOVE 'S'                   TO RSAL-ANTICIPO-PB
                ELSE
                   MOVE 'N'                   TO RSAL-ANTICIPO-PB
                END-IF

      *D*
      *         DISPLAY 'DATOS PARA CALCULO'
      *F*
                MOVE AUX-REAJUSTE-PB          TO RSAL-REAJUSTE-PB

                MOVE WS-REAJREC               TO RSAL-REAJUSTE-REC
                MOVE AUX-TIP-REAJUSTE         TO RSAL-TIP-REAJUSTE
                MOVE AUX-NANIOPER             TO RSAL-APERDIDAS
                MOVE AUX-NANIOBEN             TO RSAL-ABENEFICI
                MOVE AUX-PANTICIPO-S          TO RSAL-P-ANTICIPO
                MOVE AUX-CCUADROBAREMO-S      TO RSAL-BAREMO
                MOVE AUX-PPORCEN-REAL         TO RSAL-REAJUSTEO
                MOVE AUX-PCALCULOPB-S         TO RSAL-P-REAUSTE-PB

      *D*
      *         DISPLAY 'RSAL-REAJUSTE-REC ' RSAL-REAJUSTE-REC
      *         DISPLAY 'RSAL-TIP-REAJUSTE ' RSAL-TIP-REAJUSTE
      *         DISPLAY 'RSAL-APERDIDAS    ' RSAL-APERDIDAS
      *         DISPLAY 'RSAL-ABENEFICI    ' RSAL-ABENEFICI
      *         DISPLAY 'RSAL-P-ANTICIPO   ' RSAL-P-ANTICIPO
      *         DISPLAY 'RSAL-BAREMO       ' RSAL-BAREMO
      *         DISPLAY 'RSAL-REAJUSTEO    ' RSAL-REAJUSTEO
      *         DISPLAY 'RSAL-P-REAUSTE-PB ' RSAL-P-REAUSTE-PB
      *         DISPLAY 'RSAL-REAJUSTE-PB  ' RSAL-REAJUSTE-PB
      *F*
           IF AUX-PPORCEN-REAL    = ZEROES
              MOVE AUX-PCALCULOPB-S      TO RSAL-P-PB-APLICAR
              IF AUX-CCUADROBAREMO-S EQUAL ZEROES
              MOVE SPACES                TO RSAL-BAREMO
              END-IF
              IF AUX-PCALCULOPB-S = ZEROES
                 MOVE ZEROES TO RSAL-P-PB-APLICAR
                 IF AUX-CCUADROBAREMO-S EQUAL ZEROES
                 MOVE SPACES              TO RSAL-BAREMO
                 END-IF
              END-IF
           ELSE
              MOVE AUX-PPORCEN-REAL      TO RSAL-P-PB-APLICAR

           END-IF

                IF AUX-PPORCEN-REAL NOT = ZEROES AND
                   AUX-PCALCULOPB-S NOT = ZEROES
                   MOVE AUX-CCUADROBAREMO-S TO RSAL-BAREMO
PBTEC              MOVE AUX-PCALCULOPB-S    TO RSAL-P-PB-APLICAR
                END-IF

                MOVE AUX-PGASTOS-S            TO RSAL-P-GASTOS-PB

                COMPUTE RSAL-PRIMA-RECIBOS ROUNDED = WS-IMPORTE-205

                MOVE WS-IMPORTE-205           TO X350JPX6-PRIMA-RECIBOS

                COMPUTE RSAL-APUNTE-RECIBO ROUNDED = WS-IMPORTE-902

                MOVE WS-IMPORTE-902           TO X350JPX6-APUNTE-RECIBO

                COMPUTE RSAL-PRIMA-PERIOD ROUNDED
                        = (WS-IMPORTE-205 + WS-IMPORTE-902) * WS-ALFA
      *D*
      *         DISPLAY ' RSAL-PRIMA-PERIODANT ' RSAL-PRIMA-PERIOD
      *         DISPLAY ' X350JXX2-PCOASE-ANT  ' X350JXX2-PCOASE
      *F*


PBTECJ          COMPUTE WS-IMPORTE-PERIOD ROUNDED
                        = (WS-IMPORTE-205 +
                        WS-IMPORTE-902) * WS-ALFA

LTA             COMPUTE WS-IMPORTE-PERIOD-LTA ROUNDED
                        = (WS-IMPORTE-205 +
                           WS-IMPORTE-902) * WS-ALFA

JOSENE          COMPUTE WS-IMPORTE-205-BONO = WS-IMPORTE-205
130824          COMPUTE WS-IMPORTE-902-BONO = WS-IMPORTE-902
020924          COMPUTE WS-IMPORTE-205-BONO-NO-RENO = WS-IMPORTE-205
      *D*
      *         DISPLAY 'WS-IMPORTE-205-BONO-RENO' WS-IMPORTE-205-BONO
      *         DISPLAY 'WS-IMPORTE-902-BONO-RENO' WS-IMPORTE-902-BONO
      *F*
      *D*
      *         DISPLAY ' WS-IMPORTE-PERIOD-LTA '  WS-IMPORTE-PERIOD-LTA
      *F*

PBTECJ*            IF X350JXX2-PCOASE < 100 AND
PBTECJ*               X350JXX2-PCOASE NOT EQUAL ZEROES
PBTECJ*               COMPUTE RSAL-PRIMA-PERIOD ROUNDED
PBTECJ*                    =  WS-IMPORTE-PERIOD / (X350JXX2-PCOASE /
PBTECJ*                                            100)
      *D*
      *         DISPLAY ' RSAL-PRIMA-PERIOD-DES ' RSAL-PRIMA-PERIOD
      *         DISPLAY ' X350JXX2-PCOASE-DES   ' X350JXX2-PCOASE
      *            END-IF
      *F*

                MOVE WS-IMPORTE-PERIOD        TO X350JPX6-PRIMA-PERIOD

                COMPUTE WS-IMP-GASTOS = (WS-IMPORTE-PERIOD *
                                          AUX-PGASTOS-S) / 100

LTA             COMPUTE WS-IMP-GASTOS-LTA = (WS-IMPORTE-PERIOD *
                                             AUX-PGASTOS-S) / 100

                MOVE WS-IMP-GASTOS            TO X350JPX6-GASTOS

                COMPUTE RSAL-GASTOS ROUNDED = WS-IMP-GASTOS
      *D*
      *         DISPLAY 'JOSE '
      *         DISPLAY 'X350JXX2-KPOLIZA ' X350JXX2-KPOLIZA
      *         DISPLAY 'AUX-PGASTOS-S     ' AUX-PGASTOS-S
      *         DISPLAY 'WS-IMP-GASTOS     ' WS-IMP-GASTOS
      *         DISPLAY 'WS-IMP-GASTOS-LTA ' WS-IMP-GASTOS-LTA
      *F*

      *         DISPLAY ' WS-IMPORTE-206 ' WS-IMPORTE-206
MF001 *         MOVE WS-IMPORTE-206           TO X350JPX6-TOTAL-SINIES

                COMPUTE RSAL-TOTAL-SINIES ROUNDED = WS-IMPORTE-206
MF001 *D*
MF001 *    DISPLAY '2600-LINEA-MVTOS RSAL-LTA ' RSAL-LTA
MF001 *    DISPLAY '   TB-ACUMULADO-206 (1) ' TB-ACUMULADO-206 (1)
MF001 *    DISPLAY '   TB-ACUMULADO-206 (2) ' TB-ACUMULADO-206 (2)
MF001 *F*

MF001           IF RSAL-LTA = 'PBAN'
MF001              MOVE TB-ACUMULADO-206 (2)  TO X350JPX6-TOTAL-SINIES
MF001              COMPUTE RSAL-TOTAL-SINIES ROUNDED =
MF001                                              TB-ACUMULADO-206 (2)
MF001           ELSE
MF001              MOVE TB-ACUMULADO-206 (1)  TO X350JPX6-TOTAL-SINIES
MF001              COMPUTE RSAL-TOTAL-SINIES ROUNDED =
MF001                                              TB-ACUMULADO-206 (1)
MF001           END-IF
      *D*
      *    DISPLAY '2600-LINEA-MVTOS RSAL-TOTAL-SINIES '
      *                                             RSAL-TOTAL-SINIES
      *F*

                MOVE X350X098-IARRASTRE-S     TO RSAL-IMP-ARRAS-PERD

      *D*
      *         DISPLAY 'DATOS PARA EMPRESA'
      *F*

                IF X350JXX2-KGRUEMP NOT = SPACES
      *D*
      *           DISPLAY 'ES EMPRESA'
      *F*
                  OPEN INPUT ENTRADA-TASE
                  PERFORM 2703-LEER-TOTALES-ASEGURADO
                    UNTIL X350JXX2-KGRUEMP EQUAL
                          X350JPX4-KGRUEMP
270524*             CLOSE ENTRADA-TASE
                  MOVE X350JPX4-NASEGURADO    TO RSAL-ASE-TOT-EMPRESA

      *D*
      *           DISPLAY 'X350JXX2-KGRUEMP ' X350JXX2-KGRUEMP
      *           DISPLAY 'X350JPX4-KGRUEMP ' X350JPX4-KGRUEMP
      *           DISPLAY 'X350JPX4-PERIOD ' X350JPX4-PERIOD
      *           DISPLAY 'X350JPX4-SINIES-NR ' X350JPX4-SINIES-NR
      *           DISPLAY 'X350JPX4-GASTOS-NR ' X350JPX4-GASTOS-NR
      *F*

                  IF WS-RENOVADA = 'S'
      *D*
      *           DISPLAY ' RENOVADA '
      *F*
                  COMPUTE RSAL-PRI-TOT-EMPRESA ROUNDED
                                              = X350JPX4-PERIOD


                  COMPUTE RSAL-SIN-TOT-EMPRESA ROUNDED
                                              = X350JPX4-SINIES
      *D*
      *         DISPLAY ' X350JPX4-SINIES ' X350JPX4-SINIES
      *F*

                  COMPUTE RSAL-GASTOS-TOT-EMPR ROUNDED
                                              = X350JPX4-GASTOS
                  ELSE
      *D*
      *           DISPLAY 'VA POR EL ELSE'
      *F*
                  IF X350JPX4-PERIOD-NR IS NUMERIC
PBTECJ
                  COMPUTE RSAL-PRI-TOT-EMPRESA ROUNDED
                                              = X350JPX4-PERIOD-NR
      *                                       = X350JPX4-PERIOD


                  COMPUTE RSAL-SIN-TOT-EMPRESA ROUNDED
                                              = X350JPX4-SINIES-NR
      *                                       = X350JPX4-SINIES
      *D*
      *         DISPLAY ' X350JPX4-SINIES-NR ' X350JPX4-SINIES-NR
      *F*

                  COMPUTE RSAL-GASTOS-TOT-EMPR ROUNDED
                                              = X350JPX4-GASTOS-NR
      *                                       = X350JPX4-GASTOS
                  END-IF

      *D*
PBTECJ*       DISPLAY '   RENO          '
      *       DISPLAY 'X350JPX6-FCIERREA' X350JPX6-FCIERREA
      *       DISPLAY 'X350JPX6-FCIERREM' X350JPX6-FCIERREM
      *       DISPLAY 'X350JPX6-FCIERRED' X350JPX6-FCIERRED
      *       DISPLAY 'R350T000-FAFINAL ' R350T000-FAFINAL
      *       DISPLAY 'R350T000-FMFINAL ' R350T000-FMFINAL
      *       DISPLAY 'R350T000-FDFINAL ' R350T000-FDFINAL
      *       DISPLAY 'X350JPX4-PERIOD-NR  ' X350JPX4-PERIOD-NR
      *       DISPLAY 'X350JPX4-SINIES-NR  ' X350JPX4-SINIES-NR
      *       DISPLAY 'X350JPX4-GASTOS-NR  ' X350JPX4-GASTOS-NR
      *F*
                     IF X350JPX6-FCIERREA = R350T000-FAFINAL AND
                        X350JPX6-FCIERREM = R350T000-FMFINAL AND
                        X350JPX6-FCIERRED = R350T000-FDFINAL

                        INITIALIZE RSAL-PRI-TOT-EMPRESA
                                   RSAL-SIN-TOT-EMPRESA
                                   RSAL-GASTOS-TOT-EMPR

                        COMPUTE RSAL-PRI-TOT-EMPRESA ROUNDED
                                              = X350JPX4-PERIOD
                        COMPUTE RSAL-SIN-TOT-EMPRESA ROUNDED
                                              = X350JPX4-SINIES
                        COMPUTE RSAL-GASTOS-TOT-EMPR ROUNDED
                                              = X350JPX4-GASTOS
                     END-IF

                  END-IF
270524            CLOSE ENTRADA-TASE
                END-IF

      *D*
      *         DISPLAY 'POLIZA GLOBAL'
      *F*
                IF RSAL-PGLOBAL = '0'
                   MOVE X350JXX2-KPOLIZA         TO RSAL-POLIZA-REF1
                   MOVE '/'                      TO RSAL-SEPARADOR
                   MOVE '0000'                   TO RSAL-SUBPOL-REF1
                END-IF

                IF RSAL-POLIZA-REF1 = '0000000'
                   MOVE SPACES TO RSAL-POLIZA-REF1
                   MOVE SPACES TO RSAL-SEPARADOR
                   MOVE SPACES TO RSAL-SUBPOL-REF1
                END-IF

LUNA       MOVE RSAL-FCIERREA TO WS-FECSIN-ACIERRE
LUNA       MOVE RSAL-FCIERREM TO WS-FECSIN-MCIERRE
LUNA       MOVE RSAL-FCIERRED TO WS-FECSIN-DCIERRE

      *D*
      *          DISPLAY 'linea-mtv ' WS-FECSIN-CIERRE
      *          DISPLAY 'WS-FECSIN-CIERRE ' WS-FECSIN-CIERRE
      *          DISPLAY 'WS-FECSIN-HASTA   ' WS-FECSIN-HASTA
      *          DISPLAY 'WS-FECSIN-DESDE ' WS-FECSIN-DESDE
      *F*
LUNN       IF WS-RENOVADA = 'N' AND WS-ANTPBAN = 'S' AND RSAL-LTA = 'PB'
LUNN          MOVE WS-FECSIN-DESDE-PB TO WS-FECSIN-DESDE
           END-IF
LUNA
LUNA       MOVE WS-FECSIN-DESDE TO RSAL-FECSIN-DESDE
LUNA       IF WS-FECSIN-CIERRE < WS-FECSIN-HASTA
LUNA          MOVE WS-FECSIN-CIERRE TO RSAL-FECSIN-HASTA
LUNA       ELSE
LUNA          MOVE WS-FECSIN-HASTA TO RSAL-FECSIN-HASTA
LUNA       END-IF
LUNA            .

      *    WRITE REG-SALIDA1 FROM RSALIDA.
      ******************************************************************10650099
0124   2600-LINEA-MVTOS-FINAL-PBANT.                                    10660099
      ******************************************************************10670099
SINUSO*MF001 INITIALIZE DATOS-CTEC

                MOVE '3000'                   TO RSAL-MODELO
      *D*
      *         DISPLAY '2600-LINEA-MVTOS-FINAL-PBANT'
      *         DISPLAY 'X350JXX2-KPOLIZA  ' X350JXX2-KPOLIZA
      *         DISPLAY 'X350JXX2-KSUBPOL  ' X350JXX2-KSUBPOL
      *F*
                MOVE ZEROES TO X350JPX6-PRIMA-ANUAL

                MOVE '3000'                   TO RSAL-MODELO
                MOVE X350JXX2-FRENO-DESDE TO WS-FECHA-AUX-CNTL

                COMPUTE WS-ANIO-CNTL = WS-ANIO-CNTL + 1

      *D*
      *         DISPLAY 'WS-FECHA-AUX-CNTL ' WS-FECHA-AUX-CNTL
      *         DISPLAY 'X350JXX2-FRENO-HASTA ' X350JXX2-FRENO-AHASTA
      *F*
                IF WS-ANIO-CNTL      < X350JXX2-FRENO-AHASTA
                   MOVE WS-FECHA-AUX-CNTL TO X350JXX2-FRENO-HASTA
                END-IF

                   MOVE 'N'                   TO RSAL-RENOVADA
                   MOVE 'N'                   TO WS-RENOVADA
LTA                MOVE 'PBAN'                TO RSAL-LTA
                   MOVE 'S'                   TO WS-ANTPBAN
      *
                PERFORM CALCULAR-ANNOS-TRANS-MVT

130225          IF RSAL-LTA = 'PBAN'
130225             MOVE 1 TO WS-ALFA
130225          ELSE
                   PERFORM 3301-RECUPERAR-ALFA-MVT
130225          END-IF

      *D*
      *         DISPLAY 'POLIZA ' X350JXX2-KPOLIZA '/' X350JXX2-KSUBPOL
      *         DISPLAY 'WS-ALFA ' WS-ALFA
      *F*
                MOVE X350JXX2-CCANAL          TO RSAL-CANAL
                MOVE X350JXX2-KPOLIZA         TO RSAL-POLIZA
                MOVE X350JXX2-KSUBPOL         TO RSAL-SUBPOLIZA
                MOVE SPACES                   TO RSAL-SUSCRIP
                MOVE X350JXX2-KRAMO           TO RSAL-RAMO
                MOVE X350JXX2-MODALI          TO RSAL-MODALIDAD
                MOVE X350JXX2-VIGOR-ANUL      TO RSAL-CESTADOASEG
                MOVE '00'                     TO RSAL-PREGRUPO(1:2)
                MOVE X350JXX2-CARTINV         TO RSAL-PREGRUPO(3:2)
      *         MOVE X350JXX2-CASADA          TO RSAL-CASADO

                MOVE X350JXX2-FRENO-ADESDE    TO RSAL-FRENO-ADESDE
                MOVE X350JXX2-FRENO-MDESDE    TO RSAL-FRENO-MDESDE
                MOVE X350JXX2-FRENO-DDESDE    TO RSAL-FRENO-DDESDE

                MOVE X350JXX2-FRENO-AHASTA TO RSAL-FRENO-AHASTA
                MOVE X350JXX2-FRENO-MHASTA TO RSAL-FRENO-MHASTA
                MOVE X350JXX2-FRENO-DHASTA TO RSAL-FRENO-DHASTA

PBTEC *         MOVE '31122020'               TO WS-FECHA-CIERREF

                IF RSAL-RENOVADA = 'N'
                   MOVE X350JXX2-FRENO-AHASTA TO    RSAL-FCIERREA
                   MOVE X350JXX2-FRENO-MHASTA TO    RSAL-FCIERREM
                   MOVE X350JXX2-FRENO-DHASTA TO    RSAL-FCIERRED

LUNN               MOVE X350JXX2-FRENO-AHASTA TO    WS-FECSIN-ADESDEPB
      *                                             WS-FECSIN-ADESDE
LUNN               MOVE X350JXX2-FRENO-MHASTA TO    WS-FECSIN-MDESDEPB
      *                                             WS-FECSIN-MDESDE
LUNN               MOVE X350JXX2-FRENO-DHASTA TO    WS-FECSIN-DDESDEPB
      *                                             WS-FECSIN-DDESDE
                ELSE
                   MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
                   MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
                   MOVE R350T000-FDFINAL         TO RSAL-FCIERRED

      *            MOVE WS-FECSIN-ADESDEPB    TO    WS-FECSIN-ADESDE
      *            MOVE WS-FECSIN-MDESDEPB    TO    WS-FECSIN-MDESDE
      *            MOVE WS-FECSIN-DDESDEPB    TO    WS-FECSIN-DDESDE

                END-IF
      *D*
      *         DISPLAY 'X350JPX3-NASEG-VIGOR ' X350JPX3-NASEG-VIGOR
      *         DISPLAY 'X350JXX2-NASEG-VIGOR ' X350JXX2-NASEG-VIGOR
      *F*
                IF X350JXX2-VIGOR-ANUL  = 'A'
                   MOVE X350JXX2-ANO-ANUL     TO RSAL-FECAANULACION
                   MOVE X350JXX2-MES-ANUL     TO RSAL-FECMANULACION
                   MOVE X350JXX2-DIA-ANUL     TO RSAL-FECDANULACION
                   MOVE X350JXX2-MOTIVO-ANU   TO RSAL-MOTIVO
                END-IF

                MOVE X350JXX2-FCOBRO          TO RSAL-FORMAPAGO
      *D*
      *    DISPLAY 'X350JXX2-TIPO-COASE3: ' X350JXX2-TIPO-COASE
      *    DISPLAY 'X350JXX2-PCOASE3....: ' X350JXX2-PCOASE
      *F*
                MOVE X350JXX2-TIPO-COASE      TO RSAL-COASEGURO
021125          MOVE X350JXX2-PCOASE          TO RSAL-PCOASEGURO
                MOVE X350JXX2-PAGADOR         TO RSAL-PGLOBAL

               IF X350JXX2-CERTI EQUAL '000000' AND                     37430000
                  X350JXX2-ASEG-MECANIZADOS = 'N'                       37440000
                  MOVE 'S' TO RSAL-IMNOMINADA                           37490000

               ELSE
                  MOVE 'N' TO RSAL-IMNOMINADA                           37490000

               END-IF


                MOVE X350JXX2-NASEGURADO      TO RSAL-NASEGURADOS

PBTEC           IF X350JXX2-KGRUEMP EQUAL '001' OR
PBTEC              X350JXX2-KGRUEMP EQUAL '009'
PBTEC              MOVE SPACES TO X350JXX2-KGRUEMP
PBTEC           END-IF

200524          IF SW-SI-LTB
200524             MOVE X350M004-KGRUPEMP-BS(3:3) TO X350JXX2-KGRUEMP
200524          END-IF

200524          IF X350JXX2-KGRUEMP = ZEROS
200524            MOVE SPACES           TO X350JXX2-KGRUEMP
200524          END-IF
                   MOVE X350JXX2-KGRUEMP         TO RSAL-GEMPRESA

                IF AUX-TIP-REAJUSTE = '1' OR 'T'
                   MOVE 'S'                   TO RSAL-REAJUSTE
                ELSE
                   MOVE 'N'                   TO RSAL-REAJUSTE
                END-IF

PBTEC           IF X350X098-IANTICIPO-S > ZEROES
                   MOVE 'S'                   TO RSAL-ANTICIPO-PB
                ELSE
                   MOVE 'N'                   TO RSAL-ANTICIPO-PB
                END-IF

      *         MOVE X350M004-PREAJPB-PS      TO RSAL-REAJUSTE-PB

                MOVE WS-REAJREC               TO RSAL-REAJUSTE-REC
                MOVE AUX-TIP-REAJUSTE         TO RSAL-TIP-REAJUSTE
                MOVE AUX-NANIOPER             TO RSAL-APERDIDAS
                MOVE AUX-NANIOBEN             TO RSAL-ABENEFICI
                MOVE AUX-PANTICIPO-S          TO RSAL-P-ANTICIPO
                MOVE AUX-CCUADROBAREMO-S      TO RSAL-BAREMO
                MOVE AUX-PPORCEN-REAL         TO RSAL-REAJUSTEO
                MOVE AUX-PCALCULOPB-S         TO RSAL-P-REAUSTE-PB
                MOVE WS-ALFA                  TO RSAL-ALFA

           IF AUX-PPORCEN-REAL    = ZEROES
              MOVE AUX-PCALCULOPB-S      TO RSAL-P-PB-APLICAR
              IF AUX-CCUADROBAREMO-S EQUAL ZEROES
              MOVE SPACES                TO RSAL-BAREMO
              END-IF
              IF AUX-PCALCULOPB-S = ZEROES
                 MOVE ZEROES TO RSAL-P-PB-APLICAR
                 IF AUX-CCUADROBAREMO-S EQUAL ZEROES
                 MOVE SPACES              TO RSAL-BAREMO
                 END-IF
              END-IF
           ELSE
              MOVE AUX-PPORCEN-REAL      TO RSAL-P-PB-APLICAR

           END-IF

                IF AUX-PPORCEN-REAL NOT = ZEROES AND
                   AUX-PCALCULOPB-S NOT = ZEROES
                   MOVE AUX-CCUADROBAREMO-S TO RSAL-BAREMO
                END-IF

                MOVE AUX-PGASTOS-S            TO RSAL-P-GASTOS-PB
                COMPUTE RSAL-APUNTE-RECIBO ROUNDED = WS-IMPORTE-902
                COMPUTE RSAL-PRIMA-RECIBOS ROUNDED = WS-IMPORTE-205

                COMPUTE RSAL-PRIMA-PERIOD ROUNDED
                        = (WS-IMPORTE-205 + WS-IMPORTE-902) * WS-ALFA

                COMPUTE WS-IMPORTE-PERIOD = (WS-IMPORTE-205 +
                        WS-IMPORTE-902) * WS-ALFA


JOSENE          COMPUTE WS-IMPORTE-205-BONO = WS-IMPORTE-205
JOSENE          COMPUTE WS-IMPORTE-902-BONO = WS-IMPORTE-902
JOSENE

      *D*
      *         DISPLAY 'WS-IMPORTE-205-BONO-RENO' WS-IMPORTE-205-BONO
      *         DISPLAY 'WS-IMPORTE-902-BONO-RENO' WS-IMPORTE-902-BONO
      *F*


PBTEC *            IF X350JXX2-PCOASE < 100 AND
PBTEC *               X350JXX2-PCOASE NOT EQUAL ZEROES
PBTEC *               COMPUTE RSAL-PRIMA-PERIOD ROUNDED
PBTEC *                    =  WS-IMPORTE-PERIOD / (X350JXX2-PCOASE /
PBTEC *                                            100)
      *            END-IF

                COMPUTE WS-IMP-GASTOS = (WS-IMPORTE-PERIOD *
                              AUX-PGASTOS-S) / 100


                COMPUTE RSAL-GASTOS ROUNDED = WS-IMP-GASTOS

0124            MOVE WS-IMPORTE-206-SINI TO WS-IMPORTE-206
MF001 *         COMPUTE RSAL-TOTAL-SINIES ROUNDED = WS-IMPORTE-206
MF001           COMPUTE RSAL-TOTAL-SINIES ROUNDED = TB-ACUMULADO-206 (2)

                MOVE X350X098-IARRASTRE-S     TO RSAL-IMP-ARRAS-PERD

      *D*
      *         DISPLAY 'JOSE '
      *         DISPLAY 'X350JXX2-KPOLIZA ' X350JXX2-KPOLIZA
      *         DISPLAY 'WS-IMPORTE-206 ' WS-IMPORTE-206
      *         DISPLAY 'WS-IMPORTE-902 ' WS-IMPORTE-902
      *         DISPLAY 'RSAL-PRIMA-PERIOD ' RSAL-PRIMA-PERIOD
      *         DISPLAY 'WS-ALFA'  WS-ALFA
      *         DISPLAY 'WS-IMPORTE-PERIOD ' WS-IMPORTE-PERIOD
      *         DISPLAY 'AUX-PGASTOS-S     ' AUX-PGASTOS-S
      *         DISPLAY 'WS-IMP-GASTOS ' WS-IMP-GASTOS
      *F*
quitar*         IF X350JXX2-KGRUEMP NOT = SPACES
      *           MOVE X350JPX4-NASEGURADO    TO RSAL-ASE-TOT-EMPRESA
      *           COMPUTE RSAL-PRI-TOT-EMPRESA  ROUNDED
      *                                         =  WS-IMPORTE-205 +
      *                                            WS-IMPORTE-902
      *           COMPUTE RSAL-SIN-TOT-EMPRESA ROUNDED = WS-IMPORTE-206
      *           COMPUTE RSAL-GASTOS-TOT-EMPR ROUNDED = WS-IMP-GASTOS
      *         END-IF
      *
      *         COMPUTE WS-NETA-COASEGUR = (WS-IMPORTE-205    +
      *                                     WS-IMPORTE-902)   -
      *                                     WS-IMP-GASTOS
      *
      *         COMPUTE RSAL-NETA-COASEGUR ROUNDED = WS-NETA-COASEGUR
      *
      *         COMPUTE AUX-PB-TECNICA = (WS-NETA-COASEGUR *
      *                                    AUX-PCALCULOPB-S) / 100
      *         COMPUTE RSAL-PB-TECNICA ROUNDED
      *                                 = (WS-NETA-COASEGUR   -
      *                                    AUX-PB-TECNICA)
      *
      *         MOVE X350JPX3-PCOASE          TO AUX-PCOASE
      *
      *         COMPUTE AUX-DEV-NETCOASE = (WS-NETA-COASEGUR *
      *                                    AUX-PCOASE) / 100
      *         COMPUTE RSAL-DEV-NETCOASE ROUNDED
      *                                   = (WS-NETA-COASEGUR -
      *                                    AUX-DEV-NETCOASE)

                IF RSAL-PGLOBAL = '0'
                   MOVE X350JXX2-KPOLIZA         TO RSAL-POLIZA-REF1
                   MOVE '/'                      TO RSAL-SEPARADOR
                   MOVE '0000'                   TO RSAL-SUBPOL-REF1
                END-IF

                IF RSAL-POLIZA-REF1 = '0000000'
                   MOVE SPACES TO RSAL-POLIZA-REF1
                   MOVE SPACES TO RSAL-SEPARADOR
                   MOVE SPACES TO RSAL-SUBPOL-REF1
                END-IF

LUNA       MOVE RSAL-FCIERREA TO WS-FECSIN-ACIERRE
LUNA       MOVE RSAL-FCIERREM TO WS-FECSIN-MCIERRE
LUNA       MOVE RSAL-FCIERRED TO WS-FECSIN-DCIERRE
LUNA
LUNN       IF WS-RENOVADA = 'N' AND WS-ANTPBAN = 'S' AND RSAL-LTA = 'PB'
LUNN          MOVE WS-FECSIN-DESDE-PB TO WS-FECSIN-DESDE
           END-IF

LUNA       MOVE WS-FECSIN-DESDE TO RSAL-FECSIN-DESDE
      *D*
      *    DISPLAY 'LINEA-MVT-PBAN '
      *    DISPLAY 'WS-FECSIN-DESDE ' WS-FECSIN-DESDE
      *    DISPLAY 'WS-FECSIN-HASTA ' WS-FECSIN-HASTA
      *    DISPLAY 'WS-FECSIN-DESDE-PB ' WS-FECSIN-DESDE-PB
      *    DISPLAY 'WS-FECSIN-CIERRE   ' WS-FECSIN-CIERRE
      *F*
LUNA       IF WS-FECSIN-CIERRE < WS-FECSIN-HASTA
LUNA          MOVE WS-FECSIN-CIERRE TO RSAL-FECSIN-HASTA
LUNA       ELSE
LUNA          MOVE WS-FECSIN-HASTA TO RSAL-FECSIN-HASTA
LUNA       END-IF.

0124  *     WRITE REG-SALIDA1 FROM RSALIDA  .
      ******************************************************************10650099
LTA    2600-LINEA-MVTOS-LTA.                                            10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2600-LINEA-MVTOS-LTA '
      *    DISPLAY 'X350JXX2-KPOLIZA     ' X350JXX2-KPOLIZA
      *    DISPLAY 'X350JXX2-KSUBPOL     ' X350JXX2-KSUBPOL
      *    DISPLAY 'WS-RENOVADA ' WS-RENOVADA
      *    DISPLAY 'WS-FECHA-HASTA-TRAMO ' WS-FECHA-HASTA-TRAMO
      *F*

SINUSO*MF001 INITIALIZE DATOS-CTEC
      *               RSALIDA

           PERFORM 2702-LLAMADA-FICOL

PLURIN     MOVE X350JXX2-FRENO-HASTA TO WS-FECHA-LTA-H-REAL

           COMPUTE WS-IMPORTE-SUS  = X350JXX2-EPNETA + WS-IMPORTE-SUS
           COMPUTE WS-IMPORTE-PRI  = X350JXX2-EPNETA

                MOVE ZEROES TO X350JPX6-PRIMA-ANUAL

                IF X350JXX2-FRENO-HASTA <= R350T000-FFINAL
                   MOVE X350JXX2-FRENO-HASTA   TO WS-FEC-AUX-LTP-D
                   MOVE X350JXX2-FRENO-AHASTA  TO AUX-ANNO-LTP
                   ADD  1                      TO AUX-ANNO-LTP
                   MOVE AUX-ANNO-LTP           TO X350JXX2-FRENO-AHASTA
                   MOVE X350JXX2-FRENO-HASTA   TO WS-FEC-AUX-LTP-H
                END-IF

                PERFORM CALCULAR-ANNOS-TRANS-MVT-LTP

                PERFORM 3301-RECUPERAR-ALFA-MVT-LTP

                MOVE '3000'                   TO RSAL-MODELO

                MOVE X350JXX2-CCANAL          TO RSAL-CANAL
                                                 X350JPX6-CANAL
                MOVE X350JXX2-KPOLIZA         TO RSAL-POLIZA
                MOVE X350JXX2-KSUBPOL         TO RSAL-SUBPOLIZA
                MOVE SPACES                   TO RSAL-SUSCRIP
                MOVE X350JXX2-KRAMO           TO RSAL-RAMO
                                                 X350JPX6-RAMO
                MOVE X350JXX2-MODALI          TO RSAL-MODALIDAD
                                                 X350JPX6-MODALIDAD
                MOVE X350JXX2-VIGOR-ANUL      TO RSAL-CESTADOASEG
                MOVE '00'                     TO RSAL-PREGRUPO(1:2)
                MOVE X350JXX2-CARTINV         TO RSAL-PREGRUPO(3:2)

      *         MOVE WS-ANNO-DESDE-TRAMO      TO RSAL-FRENO-ADESDE
      *         MOVE WS-MES-DESDE-TRAMO       TO RSAL-FRENO-MDESDE
      *         MOVE WS-DIA-DESDE-TRAMO       TO RSAL-FRENO-DDESDE
PLURIN       MOVE WS-FRENO-DESDE-APLURI-ACTUAL TO RSAL-FRENO-ADESDE
PLURIN       MOVE WS-FRENO-DESDE-MPLURI-ACTUAL TO RSAL-FRENO-MDESDE
PLURIN       MOVE WS-FRENO-DESDE-DPLURI-ACTUAL TO RSAL-FRENO-DDESDE

RF001         IF WS-FECHA-HASTA-TRAMO EQUAL ZEROES
RF001            MOVE WS-FRENO-HASTA-PLURI-ACTUAL TO WS-FECSIN-HASTA
RF001         END-IF

RF001        IF WS-ANNO-HASTA-TRAMO EQUAL ZEROES
RF001           MOVE WS-FRENO-HASTA-APLURI-ACTUAL TO WS-ANNO-HASTA-TRAMO
RF001           MOVE WS-FRENO-HASTA-MPLURI-ACTUAL TO WS-MES-HASTA-TRAMO
RF001           MOVE WS-FRENO-HASTA-DPLURI-ACTUAL TO WS-DIA-HASTA-TRAMO
RF001        END-IF

                MOVE WS-ANNO-HASTA-TRAMO      TO RSAL-FRENO-AHASTA
                MOVE WS-MES-HASTA-TRAMO       TO RSAL-FRENO-MHASTA
                MOVE WS-DIA-HASTA-TRAMO       TO RSAL-FRENO-DHASTA

JOSEN           IF WS-FECHA-HASTA-TRAMO < R350T000-FFINAL
JOSEN              MOVE WS-ANNO-HASTA-TRAMO TO RSAL-FCIERREA
                                                 X350JPX6-FCIERREA
JOSEN              MOVE WS-MES-HASTA-TRAMO  TO RSAL-FCIERREM
                                                 X350JPX6-FCIERREM
JOSEN              MOVE WS-DIA-HASTA-TRAMO  TO RSAL-FCIERRED
                                                 X350JPX6-FCIERRED
JOSEN           ELSE
                   MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
                                                 X350JPX6-FCIERREA
                   MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
                                                 X350JPX6-FCIERREM
                   MOVE R350T000-FDFINAL         TO RSAL-FCIERRED
                                                 X350JPX6-FCIERRED
JOSEN           END-IF
      *         MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
      *                                          X350JPX6-FCIERREA
      *         MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
      *                                          X350JPX6-FCIERREM
      *         MOVE R350T000-FDFINAL         TO RSAL-FCIERRED
      *                                          X350JPX6-FCIERRED
                MOVE 'PLURI'                  TO RSAL-LTA

OJO   *         MOVE SPACES TO WS-RENOVADA
PLURIN          IF WS-FECHA-LTA-H-REAL    >  R350T000-FFINAL
PLURIN             MOVE 'S'                   TO WS-RENOVADA
PLURIN          ELSE
PLURIN             MOVE 'N'                   TO WS-RENOVADA
PLURIN          END-IF

PLURIN**        MOVE 'S'                      TO WS-RENOVADA
PLURIN          MOVE WS-RENOVADA              TO RSAL-RENOVADA

FECHAS*08/04/2024 - PARA IGUALAR CON B350JL03 - comentado con Jose
FECHAS*08/04/2024 -  fase1 de momento todos los PLURIS son renovados
FECHAS          MOVE WS-RENOVADA              TO X350JPX6-RENOVADA
FECHAS*         IF WS-FECHA-LTA-H         >  R350T000-FFINAL
FECHAS*         OR WS-FECHA-LTA-H         =  R350T000-FFINAL
FECHAS*            MOVE 'S'                   TO RSAL-RENOVADA
FECHAS*                                          X350JPX6-RENOVADA
FECHAS*            MOVE 'S' TO WS-RENOVADA
FECHAS*         ELSE
FECHAS*            MOVE 'N'                   TO RSAL-RENOVADA
FECHAS*                                          X350JPX6-RENOVADA
FECHAS*            MOVE 'N' TO WS-RENOVADA
FECHAS*         END-IF

                IF X350JXX2-VIGOR-ANUL  = 'A'
                   MOVE X350JXX2-ANO-ANUL     TO RSAL-FECAANULACION
                   MOVE X350JXX2-MES-ANUL     TO RSAL-FECMANULACION
                   MOVE X350JXX2-DIA-ANUL     TO RSAL-FECDANULACION
                   MOVE X350JXX2-MOTIVO-ANU   TO RSAL-MOTIVO
                END-IF

                MOVE X350JXX2-FCOBRO          TO RSAL-FORMAPAGO
                MOVE X350JXX2-TIPO-COASE      TO RSAL-COASEGURO
                MOVE X350JXX2-PCOASE          TO RSAL-PCOASEGURO
                MOVE X350JXX2-PAGADOR         TO RSAL-PGLOBAL

                IF X350JXX2-CERTI EQUAL '000000' AND                    37430000
                   X350JXX2-ASEG-MECANIZADOS = 'N'                      37440000
                   MOVE 'S' TO RSAL-IMNOMINADA                          37490000

                ELSE
                   MOVE 'N' TO RSAL-IMNOMINADA                          37490000

                END-IF

                MOVE X350JXX2-NASEGURADO      TO RSAL-NASEGURADOS
      *D*
      *         DISPLAY 'WS-KGRUPEMP-PS-ACTUAL ' WS-KGRUPEMP-PS-ACTUAL
      *         DISPLAY 'X350M004-KGRUPEMP-PS' X350M004-KGRUPEMP-PS(3:3)
      *F*
EMPMAR*         MOVE X350JXX2-KGRUEMP         TO RSAL-GEMPRESA
130624          MOVE WS-KGRUPEMP-PS-ACTUAL TO X350M004-KGRUPEMP-PS
EMPMAR          IF X350M004-KGRUPEMP-PS = ZEROS
EMPMAR             MOVE SPACES                TO X350M004-KGRUPEMP-PS
EMPMAR          END-IF
EMPMAR          MOVE X350M004-KGRUPEMP-PS(3:3) TO RSAL-GEMPRESA

                IF X350X098-IANTICIPO-S > ZEROES
                   MOVE 'S'                   TO RSAL-ANTICIPO-PB
                ELSE
                   MOVE 'N'                   TO RSAL-ANTICIPO-PB
                END-IF

                MOVE AUX-CCOBON-REAJUS        TO RSAL-REAJUSTE
                MOVE AUX-REAJUSTE-PB          TO RSAL-REAJUSTE-PB
                MOVE WS-REAJREC               TO RSAL-REAJUSTE-REC
                MOVE AUX-TIP-REAJUSTE         TO RSAL-TIP-REAJUSTE
                MOVE AUX-NANIOPER             TO RSAL-APERDIDAS
                MOVE AUX-NANIOBEN             TO RSAL-ABENEFICI
                MOVE AUX-PANTICIPO-S          TO RSAL-P-ANTICIPO
                MOVE AUX-CCUADROBAREMO-S      TO RSAL-BAREMO
                MOVE AUX-PPORCEN-REAL         TO RSAL-REAJUSTEO
                MOVE AUX-PCALCULOPB-S         TO RSAL-P-REAUSTE-PB

      *    IF AUX-PPORCEN-REAL    = ZEROES
      *       MOVE AUX-PCALCULOPB-S      TO RSAL-P-PB-APLICAR
      *       IF AUX-CCUADROBAREMO-S EQUAL ZEROES
      *       MOVE SPACES                TO RSAL-BAREMO
      *       END-IF
      *       IF AUX-PCALCULOPB-S = ZEROES
      *          MOVE ZEROES TO RSAL-P-PB-APLICAR
      *          IF AUX-CCUADROBAREMO-S EQUAL ZEROES
      *          MOVE SPACES              TO RSAL-BAREMO
      *          END-IF
      *       END-IF
      *    ELSE
      *
      *    END-IF

      *         IF AUX-PPORCEN-REAL NOT = ZEROES AND
      *            AUX-PCALCULOPB-S NOT = ZEROES
      *            MOVE AUX-CCUADROBAREMO-S TO RSAL-BAREMO
      *            MOVE AUX-PCALCULOPB-S    TO RSAL-P-PB-APLICAR
      *         END-IF

210524*    MOVE X350M004-PREAJPB-PS      TO RSAL-P-PB-APLICAR
210524*    MOVE X350M004-PREAJPB-PS      TO RSAL-P-REAUSTE-PB
210524*    MOVE X350M004-PREAJPB-PS      TO AUX-PCALCULOPB-S
210524*    MOVE X350M004-PREAJPB-PS      TO RSAL-P-GASTOS-PB
210524     MOVE WS-PRREAPB-PS-ACTUAL     TO RSAL-P-PB-APLICAR
210524     MOVE WS-PRREAPB-PS-ACTUAL     TO RSAL-P-REAUSTE-PB
210524     MOVE WS-PRREAPB-PS-ACTUAL     TO AUX-PCALCULOPB-S
210524     MOVE WS-PGASTOS-PS-ACTUAL     TO RSAL-P-GASTOS-PB
      *D*
      *         DISPLAY 'RSAL-P-PB-APLICAR ' RSAL-P-PB-APLICAR
      *         DISPLAY 'RSAL-P-GASTOS-PB ' RSAL-P-GASTOS-PB
      *         DISPLAY 'WS-ANNO-DESDE-TRAMO ' WS-ANNO-DESDE-TRAMO
      *         DISPLAY 'WS-MES-DESDE-TRAMO  ' WS-MES-DESDE-TRAMO
      *         DISPLAY 'WS-DIA-DESDE-TRAMO  ' WS-DIA-DESDE-TRAMO
      *F*

                COMPUTE RSAL-PRIMA-RECIBOS ROUNDED = WS-IMPORTE-205

311224          IF WS-RENOVADA = 'N' AND SW-SI-LTA-FINAL
                   COMPUTE RSAL-PRIMA-RECIBOS ROUNDED = X350JXX2-EPNETA
                END-IF

0124            IF WS-RENOVADA = 'N' AND SW-SI-LTA-MITAD

      *D*
      *         DISPLAY ' ENTRO IF-SI-MITAD-PARA BONO '
      *F*
0124               COMPUTE RSAL-PRIMA-RECIBOS = X350JXX2-EPNETA
0124                                                 + WS-IMPORTE-205
0124            END-IF

                MOVE RSAL-PRIMA-RECIBOS       TO X350JPX6-PRIMA-RECIBOS

                COMPUTE RSAL-APUNTE-RECIBO ROUNDED = WS-IMPORTE-902

                MOVE WS-IMPORTE-902           TO X350JPX6-APUNTE-RECIBO

      *D*
      *         DISPLAY 'WS-IMP-PERI-CAL1 ' WS-IMP-PERI-CAL1
      *         DISPLAY 'WS-IMP-PERI-CAL2 ' WS-IMP-PERI-CAL2
      *         DISPLAY 'WS-IMP-PERI-CAL3 ' WS-IMP-PERI-CAL3
      *         DISPLAY 'WS-ALFA          ' WS-ALFA
      *F*
240524*         IF WS-IMP-PERI-CAL3 EQUAL ZEROES
240524          IF WS-IMP-PERI-CAL3 EQUAL ZEROES AND WS-RENOVADA = 'S'
240524             MOVE 1 TO WS-ALFA
240524                       RSAL-ALFA
                ELSE
                   MOVE WS-ALFA TO RSAL-ALFA
240524          END-IF


PLURIN          IF WS-RENOVADA = 'N' AND SW-SI-LTA-MITAD
PLURIN             COMPUTE WS-EPNETA-NO-RENO-PLURI = X350JXX2-EPNETA
PLURIN                                               * WS-ALFA
PLURIN          END-IF





                COMPUTE WS-IMP-PERI-CAL3   ROUNDED
                        = (WS-IMP-PERI-CAL3) * WS-ALFA

                MOVE WS-ALFA TO WS-ALFA-PLU-BON
      *D*
      *         DISPLAY 'WS-IMP-PERI-CAL3 ' WS-IMP-PERI-CAL3
      *F*
                COMPUTE WS-IMP-PERI-CALC = WS-IMP-PERI-CAL1 +
                        WS-IMP-PERI-CAL2 + WS-IMP-PERI-CAL3

                MOVE ZEROES           TO WS-IMPORTE-PLUPE
                MOVE WS-IMP-PERI-CALC TO WS-IMPORTE-PLUPE
                MOVE ZEROES           TO WS-IMPORTE-PLURE
                MOVE WS-IMPORTE-205   TO WS-IMPORTE-PLURE

      *         MOVE WS-ALFA                  TO RSAL-ALFA
      *D*
      *         DISPLAY 'WS-IMP-PERI-CALC ' WS-IMP-PERI-CALC
      *F*

PLURIN          IF WS-RENOVADA = 'N' AND SW-SI-LTA-MITAD
PLURIN
PLURIN          COMPUTE RSAL-PRIMA-PERIOD ROUNDED
PLURIN                  = WS-IMP-PERI-CALC + WS-EPNETA-NO-RENO-PLURI

PLURIN          COMPUTE WS-IMPORTE-PERIOD ROUNDED
PLURIN                  = WS-IMP-PERI-CALC + WS-EPNETA-NO-RENO-PLURI
PLURIN
PLURIN          COMPUTE WS-IMPORTE-PERIOD-LTA ROUNDED
PLURIN                  = WS-IMP-PERI-CALC + WS-EPNETA-NO-RENO-PLURI
PLURIN          ELSE

                COMPUTE RSAL-PRIMA-PERIOD ROUNDED
                        = WS-IMP-PERI-CALC

                COMPUTE WS-IMPORTE-PERIOD ROUNDED
                        = WS-IMP-PERI-CALC

LTA             COMPUTE WS-IMPORTE-PERIOD-LTA ROUNDED
                        = WS-IMP-PERI-CALC
PLURIN          END-IF

      *D*
      *         DISPLAY 'ANTES-DE-CALCULO '
      *         DISPLAY 'WS-RENOVADA ' WS-RENOVADA
      *         DISPLAY 'RSAL-PRIMA-PERIOD ' RSAL-PRIMA-PERIOD
      *F*
311224        IF WS-RENOVADA = 'N' AND SW-SI-LTA-FINAL
      *D*
      *         DISPLAY 'ENTRO-IF '
      *F*
PLURIN          COMPUTE RSAL-PRIMA-PERIOD ROUNDED
PLURIN                  = X350JXX2-EPNETA * WS-ALFA
PLURIN
PLURIN          COMPUTE WS-IMPORTE-PERIOD-LTA ROUNDED
PLURIN                  = X350JXX2-EPNETA * WS-ALFA

PLURIN          COMPUTE WS-IMPORTE-PERIOD ROUNDED
PLURIN                  = X350JXX2-EPNETA * WS-ALFA

                MOVE WS-IMPORTE-PERIOD-LTA TO WS-IMPORTE-PLUPE
                MOVE X350JXX2-EPNETA     TO WS-IMPORTE-PLURE
      *D*
      *         DISPLAY 'DESPUES-CALCULO   '
      *         DISPLAY 'RSAL-PRIMA-PERIOD ' RSAL-PRIMA-PERIOD
      *F*
              END-IF
      *D*
      *         DISPLAY ' WS-IMPORTE-PERIOD-LTA '  WS-IMPORTE-PERIOD-LTA
      *         DISPLAY ' WS-IMPORTE-PLUPE      '  WS-IMPORTE-PLUPE
      *         DISPLAY ' WS-IMPORTE-PERIOD     '  WS-IMPORTE-PERIOD
      *F*


                MOVE WS-IMPORTE-PERIOD        TO X350JPX6-PRIMA-PERIOD

                COMPUTE WS-IMP-GASTOS = (WS-IMPORTE-PERIOD *
210524*                             X350M004-PGASTOS-PS) / 100
210524                            WS-PGASTOS-PS-ACTUAL) / 100

LTA             COMPUTE WS-IMP-GASTOS-LTA = (WS-IMPORTE-PERIOD *
210524*                             X350M004-PGASTOS-PS) / 100
210524                            WS-PGASTOS-PS-ACTUAL) / 100

                MOVE WS-IMP-GASTOS            TO X350JPX6-GASTOS
      *D*
      *         DISPLAY ' WS-IMP-GASTOS         '  WS-IMP-GASTOS
      *         DISPLAY ' WS-IMP-GASTOS-LTA     '  WS-IMP-GASTOS-LTA
      *F*

                COMPUTE RSAL-GASTOS ROUNDED = WS-IMP-GASTOS

                MOVE ZEROES           TO WS-IMPORTE-PLUGA
                MOVE WS-IMP-GASTOS    TO WS-IMPORTE-PLUGA

                IF SW-SI-LTPA
                   CONTINUE
                ELSE
                   MOVE WS-IMPORTE-206-SINI TO WS-IMPORTE-206
                END-IF

0124            IF SW-SI-LTA-FINAL AND SW-SW-LTPA EQUAL 'N'
0124                              AND WS-RENOVADA-LTA EQUAL 'N'
0124               INITIALIZE WS-IMPORTE-206
0124            END-IF

MF001 *         MOVE WS-IMPORTE-206           TO X350JPX6-TOTAL-SINIES
MF001           MOVE TB-ACUMULADO-206 (3)     TO X350JPX6-TOTAL-SINIES

MF001 *         COMPUTE RSAL-TOTAL-SINIES ROUNDED = WS-IMPORTE-206
                COMPUTE RSAL-TOTAL-SINIES ROUNDED = TB-ACUMULADO-206 (3)

                MOVE WS-IMPORTE-206 TO WS-IMPORTE-206-PLU-BON
      *D*
      *         DISPLAY ' WS-IMPORTE-206        '  WS-IMPORTE-206
      *         DISPLAY ' WS-IMPORTE-206-PLU-BON' WS-IMPORTE-206-PLU-BON
      *         DISPLAY ' WS-IMP-GASTOS         '  WS-IMP-GASTOS
      *         DISPLAY ' X350M004-KGRUPEMP-PS  '  X350M004-KGRUPEMP-PS
      *         DISPLAY ' X350JPX4-KGRUEMP      '  X350JPX4-KGRUEMP
      *         DISPLAY ' WS-IMPORTE-PLURE      '  WS-IMPORTE-PLURE
      *F*

                MOVE X350X098-IARRASTRE-S     TO RSAL-IMP-ARRAS-PERD

                MOVE SPACES TO X350JPX4-KGRUEMP
                MOVE SPACES TO X350JPX5-GEMPRESA

      *D*
      *         DISPLAY ' WS-IMPORTE-206        '  WS-IMPORTE-206
      *         DISPLAY ' WS-IMPORTE-206-PLU-BON' WS-IMPORTE-206-PLU-BON
      *         DISPLAY ' WS-IMP-GASTOS         '  WS-IMP-GASTOS
      *         DISPLAY ' X350JPX5-GEMPRESA     '  X350JPX5-GEMPRESA
      *         DISPLAY ' KGRUPEMP-PS ' X350M004-KGRUPEMP-PS(3:3)
      *         DISPLAY ' X350JPX4-KGRUEMP ' X350JPX4-KGRUEMP
      *F*


RF001         IF SW-SI-LTA-FINAL AND SW-SW-LTPA EQUAL 'N'
RF001                         AND WS-RENOVADA-LTA EQUAL 'N'

RF001           MOVE X350JXX2-NASEGURADO TO RSAL-ASE-TOT-EMPRESA
RF001           ELSE

EMPMAR*         IF X350JXX2-KGRUEMP NOT = SPACES
EMPMAR          IF X350M004-KGRUPEMP-PS NOT = SPACES
                  OPEN INPUT ENTRADA-TASE
                  PERFORM 2703-LEER-TOTALES-ASEGURADO
EMPMAR**            UNTIL X350JXX2-KGRUEMP EQUAL
EMPMAR              UNTIL X350M004-KGRUPEMP-PS(3:3) EQUAL
                          X350JPX4-KGRUEMP
                    CLOSE ENTRADA-TASE

                  MOVE X350JPX4-NASEGURADO    TO RSAL-ASE-TOT-EMPRESA
RF001           END-IF
              END-IF
                  OPEN INPUT ENTRADA-TASE-LTP
                  PERFORM    2703-LEER-TOTALES-LTP
EMPMAR*****       UNTIL      X350JXX2-KGRUEMP EQUAL
EMPMAR            UNTIL X350M004-KGRUPEMP-PS(3:3) EQUAL
                             X350JPX5-GEMPRESA
                  CLOSE      ENTRADA-TASE-LTP
      *D*
      *          DISPLAY 'X350JPX5-TOT-SINIES ' X350JPX5-TOTAL-SINIES
      *          DISPLAY 'X350JPX5-PRIMA-PERIOD ' X350JPX5-PRIMA-PERIOD
      *F*
                  COMPUTE RSAL-PRI-TOT-EMPRESA ROUNDED
                                          = X350JPX5-PRIMA-PERIOD


                  COMPUTE RSAL-SIN-TOT-EMPRESA ROUNDED
                                          = X350JPX5-TOTAL-SINIES

                  COMPUTE RSAL-GASTOS-TOT-EMPR =
210524*           (X350JPX5-PRIMA-PERIOD * X350M004-PGASTOS-PS) / 100
210524            (X350JPX5-PRIMA-PERIOD * WS-PGASTOS-PS-ACTUAL) / 100

210524*           COMPUTE RSAL-PBPAGA-TOT-EMPR ROUNDED
210524*                                   = X350JPX5-PBPAGADA-EMP


      *****     END-IF

                IF RSAL-PGLOBAL = '0'
                   MOVE X350JXX2-KPOLIZA         TO RSAL-POLIZA-REF1
                   MOVE '/'                      TO RSAL-SEPARADOR
                   MOVE '0000'                   TO RSAL-SUBPOL-REF1
                END-IF

                IF RSAL-POLIZA-REF1 = '0000000'
                   MOVE SPACES TO RSAL-POLIZA-REF1
                   MOVE SPACES TO RSAL-SEPARADOR
                   MOVE SPACES TO RSAL-SUBPOL-REF1
                END-IF

      *D*
      *          DISPLAY 'RSAL-FECHA CIERRE: ' WS-FECSIN-CIERRE
      *          DISPLAY 'RSAL-FECHA HASTA: ' WS-FECSIN-HASTA
      *F*
LUNA       MOVE RSAL-FCIERREA TO WS-FECSIN-ACIERRE
LUNA       MOVE RSAL-FCIERREM TO WS-FECSIN-MCIERRE
LUNA       MOVE RSAL-FCIERRED TO WS-FECSIN-DCIERRE
LUNA
LUNA       MOVE WS-FECSIN-DESDE TO RSAL-FECSIN-DESDE
LUNA       IF WS-FECSIN-CIERRE < WS-FECSIN-HASTA
LUNA          MOVE WS-FECSIN-CIERRE TO RSAL-FECSIN-HASTA
LUNA       ELSE
LUNA          MOVE WS-FECSIN-HASTA TO RSAL-FECSIN-HASTA
LUNA       END-IF.


      ******************************************************************10650099
       2701-LLAMADA-RB03031.                                            10660099
      ******************************************************************10670099

      *D*
      *    DISPLAY '2701-LLAMADA-RB03031'
      *F*
           MOVE X350JXX2-KPOLIZA    TO WS-POLIZA-3130
           MOVE X350JXX2-KSUBPOL    TO WS-SUBPOL-3130

           OBTAIN RB03130 WITHIN R03-INX-130-1
                          USING  WS-CLAVE-3130
PBTEC      ON DB-REC-NOT-FOUND
              MOVE 'N'  TO HAY-RB03130
               IF DB-STATUS-OK
                  CONTINUE
               ELSE
                  PERFORM 2700-LLAMADA-B350M099
                  INITIALIZE RB03130
                  INITIALIZE RB03131
               END-IF

           END-IF.

              MOVE X350JXX2-FRENO-DESDE TO WS-CLAVE-3131
              OBTAIN RB03131 WITHIN R03-130-131-1
                             USING  WS-CLAVE-3131
              ON DB-REC-NOT-FOUND
                 OBTAIN FIRST RB03131 WITHIN R03-130-131-1
                    ON DB-END-OF-SET
                       INITIALIZE RB03131
                 END-IF
              END-IF
                    IF X350JXX2-KPOLIZA NOT EQUAL RB03130-KSG-POLIZA
                    OR  X350JXX2-KSUBPOL NOT EQUAL
                        RB03130-KSG-SUBPOL

                       INITIALIZE RB03130
                       INITIALIZE RB03131

                            MOVE 'N'  TO HAY-RB03131
                    ELSE
               MOVE RB03131-KSGREA-CUADRO TO AUX-CCUADROBAREMO-S
               MOVE RB03131-PSGREA        TO AUX-PCALCULOPB-S
               MOVE RB03131-PSGREA-GASTOS TO AUX-PGASTOS-S
               MOVE RB03130-CSGREA-PB     TO AUX-REAJUSTE-PB
               MOVE RB03130-TSGREA        TO AUX-TIP-REAJUSTE
               MOVE RB03131-NSG-ANIOPER   TO AUX-NANIOPER
               MOVE RB03131-NSG-ANIOBEN   TO AUX-NANIOBEN
      *
      *D*      DISPLAY ' RB03130-KSG-POLIZA    ' RB03130-KSG-POLIZA
      *        DISPLAY ' RB03130-KSG-POLIZA    ' RB03130-KSG-POLIZA
      *        DISPLAY ' X350JXX2-KPOLIZA      ' X350JXX2-KPOLIZA
      *        DISPLAY ' RB03131-KSGREA-CUADRO ' RB03131-KSGREA-CUADRO
      *        DISPLAY ' RB03131-PSGREA        ' RB03131-PSGREA
      *        DISPLAY ' RB03131-PSGREA-GASTOS ' RB03131-PSGREA-GASTOS
      *        DISPLAY ' AUX-PGASTOS-S         ' AUX-PGASTOS-S
      *        DISPLAY ' RB03130-CSGREA-PB     ' RB03130-CSGREA-PB
      *        DISPLAY ' RB03130-TSGREA        ' RB03130-TSGREA
      *        DISPLAY ' RB03131-NSG-ANIOPER   ' RB03131-NSG-ANIOPER
      *        DISPLAY ' RB03131-NSG-ANIOBEN   ' RB03131-NSG-ANIOBEN
      *F*      DISPLAY ' RB03131-NSG-ANIOBEN   ' RB03131-NSG-ANIOBEN

                     IF AUX-CCUADROBAREMO-S EQUAL ZEROS
                       MOVE SPACES TO AUX-CCUADROBAREMO-S
                       MOVE 'POL CON BAREMO 0' TO X350J000-DESC-ERROR
                       MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                       MOVE 'B350JA12'          TO X350J000-ID-MODULO
                       MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                       MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                       MOVE 'W'                 TO X350J000-NIVEL
                       MOVE '01'                TO X350J000-ID-ERROR-MOD
                       PERFORM 2400-ESCRIBIR-ERRORES
                     END-IF
                    END-IF.
      ******************************************************************10650099
       OBTENER-RB03631-RR.
      ******************************************************************10650099

           MOVE X350JXX2-KPOLIZA      TO WS-POLIZA-3631
           MOVE X350JXX2-KSUBPOL      TO WS-SUBPOL-3631


           OBTAIN RB03631 WITHIN R03-INX-631-1 USING WS-CLAVE-3631
           ON DB-REC-NOT-FOUND
                 OBTAIN NEXT RB03631 WITHIN R03-INX-631-1
                   ON DB-END-OF-SET
                     INITIALIZE RB03631
                   END-IF
           END-IF.


              IF DB-STATUS-OK
                 IF X350JXX2-KPOLIZA NOT = RB03631-KCOTOM-POLIZA
                    MOVE SPACES TO RB03631-CRENOV-REAJREC
                 END-IF
                 MOVE RB03631-CRENOV-REAJREC TO WS-REAJREC

              ELSE
                 PERFORM 2700-LLAMADA-B350M100

              END-IF.

      ******************************************************************10650099
       2700-LLAMADA-B350M100.                                           10660099
      ******************************************************************10670099
      *D*
      *       DISPLAY ' ENTRO A LLAMADA-B350M100 '
      *F*
           MOVE X350JXX2-KPOLIZA TO X350M100-KPOLIZA-E
           MOVE X350JXX2-KPOLIZA TO X350M100-SUBPOL-E

           CALL B350M100 USING WS-X350M100

           IF X350M100-CRETORNO-S = '00'
              MOVE X350M100-CREPARTOREAJ-S TO WS-REAJREC

           ELSE
              MOVE SPACES TO WS-REAJREC

           END-IF.

      ******************************************************************10650099
       2700-LLAMADA-B350M099.                                           10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2700-LLAMADA-B350M099'
      *F*
           INITIALIZE WS-X350X098
            REPLACING ALPHANUMERIC BY SPACES
                           NUMERIC BY ZEROES

           MOVE X350JXX2-KPOLIZA            TO X350X098-KPOLIZA-E
           MOVE X350JXX2-KSUBPOL            TO X350X098-KSUBPOLIZA-E
           MOVE '000'                       TO X350X098-KGRUPO-E
PBTEC      MOVE R350T000-FFINAL             TO X350X098-FCONSULTA-E
           MOVE SPACE                       TO AUX-CCUADROBAREMO-S

           CALL B350M099 USING WS-X350X098

           IF X350X098-CRETORNO-S NOT = ZEROES
      *D*
      *    DISPLAY 'X350X098-CRETORNO-S  ' X350X098-CRETORNO-S
      *F*
              MOVE SPACE TO AUX-CCUADROBAREMO-S
           ELSE
      *    DISPLAY 'X350X098-CCUADROBAREMO-S ' X350X098-CCUADROBAREMO-S
              MOVE X350X098-CCUADROBAREMO-S  TO AUX-CCUADROBAREMO-S
              MOVE X350X098-PCALCULOPB-S     TO AUX-PCALCULOPB-S
              MOVE X350X098-PGASTOS-S        TO AUX-PGASTOS-S
              MOVE X350X098-CPERIODICIDAD-S  TO AUX-TIP-REAJUSTE
      *D*
      *    DISPLAY 'X350X098-PGASTOS-S    ' X350X098-PCALCULOPB-S
      *    DISPLAY 'AUX-PGASTOS-S         ' AUX-PGASTOS-S
      *F*
              MOVE X350X098-CAPLICATECNICA-S TO AUX-REAJUSTE-PB
              MOVE X350X098-NANIOBEN-S       TO AUX-NANIOBEN
              MOVE X350X098-NANIOPER-S       TO AUX-NANIOPER

              MOVE 'ERROR AL RECUPERAR BAREMO NIIF17'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'B350M099'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE X350X098-CRETORNO-S TO X350J000-ID-ERROR-MOD

              PERFORM 2400-ESCRIBIR-ERRORES

           END-IF
           .
      ******************************************************************
       2702-CONSULTAR-BAREMO.
      ******************************************************************
      *D*
DSPLYS*    DISPLAY '2702-CONSULTAR-BAREMO'
DSPLYS*    DISPLAY 'AUX-CCUADROBAREMO-S  ' AUX-CCUADROBAREMO-S
      *F*
           INITIALIZE WS-X520X031

           MOVE AUX-CCUADROBAREMO-S            TO X520X031-KBAREMO-E

           CALL B510M031 USING WS-X520X031

      *D*
DSPLYS*    DISPLAY 'X350JXX2-KPOLIZA        ' X350JXX2-KPOLIZA
DSPLYS*    DISPLAY 'X350JXX2-KSUBPOL        ' X350JXX2-KSUBPOL
DSPLYS*    DISPLAY 'X520X031-CRETORNO       ' X520X031-CRETORNO
      *    DISPLAY 'LISTABAREMO-OCC-S       ' X520X031-LISTABAREMO-OCC-S
      *    DISPLAY 'X520X031-KBAREMO-E      ' X520X031-KBAREMO-E
      *F*

           IF X520X031-CRETORNO EQUAL ZEROES AND
PBTECJ        X520X031-KBAREMO-E NOT EQUAL ZEROES
              MOVE X520X031-LISTABAREMO-OCC-S
                       TO AUX-LISTABAREMO
      *D*
      *       DISPLAY 'XN-J              ' XN-J
      *       DISPLAY 'LISTABAREMO-OCC-S ' X520X031-LISTABAREMO-OCC-S
      *F*
              PERFORM VARYING XN-J FROM 1 BY 1
                UNTIL XN-J > X520X031-LISTABAREMO-OCC-S
      *               OR AUX-PPORCEN-REAL > 1
      *D*
      *          DISPLAY 'XN-J              ' XN-J
      *          DISPLAY 'LISTABAREMO-OCC-S ' X520X031-LISTABAREMO-OCC-S
      *F*

                 MOVE X520X031-NASEGURADOSDESDE-S(XN-J)
                        TO AUX-NASEGURADOSDESDE(XN-J)
                 MOVE X520X031-NASEGURADOSHASTA-S(XN-J)
                        TO AUX-NASEGURADOSHASTA(XN-J)
                 MOVE X520X031-PPORCENTAJEPB-S(XN-J)
                        TO AUX-PPORCENTAJEPB(XN-J)

      *         IF  X350JXX2-NASEGURADO  >= AUX-NASEGURADOSDESDE(XN-J)
                IF  WS-AUX-ASEGURADOS    >= AUX-NASEGURADOSDESDE(XN-J)
      *         AND X350JXX2-NASEGURADO  <= AUX-NASEGURADOSHASTA(XN-J)
                AND WS-AUX-ASEGURADOS    <= AUX-NASEGURADOSHASTA(XN-J)
      *D*
DSPLYS*    DISPLAY 'X350JXX2-KPOLIZA        ' X350JXX2-KPOLIZA
DSPLYS*    DISPLAY 'X350JXX2-KSUBPOL        ' X350JXX2-KSUBPOL
DSPLYS*    DISPLAY 'AUX-PPORCENTAJEPB(XN-J) ' AUX-PPORCENTAJEPB(XN-J)
DSPLYS*    DISPLAY 'X350JXX2-NASEG-VIGOR    ' X350JXX2-NASEG-VIGOR
      *F*
                    MOVE AUX-PPORCENTAJEPB(XN-J) TO AUX-PPORCEN-REAL
                END-IF
              END-PERFORM
           ELSE

              MOVE 'ERROR AL CONSULTAR BAREMO NIIF17'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'B520M031'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE X520X031-CRETORNO   TO X350J000-ID-ERROR-MOD

              PERFORM 2400-ESCRIBIR-ERRORES

           END-IF.
      ******************************************************************10650099
       2800-TRATAR-RENO.                                                10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2800-TRATAR-RENO'
      *F*

           PERFORM 2100-RECUPERAR-DATOS-SI-RENO
           .
      ******************************************************************10650099
       2900-TRATAR-NO-RENO.                                             10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2900-TRATAR-NO-RENO'
      *F*

           PERFORM 2100-RECUPERAR-DATOS-NO-RENO

           ADD 1          TO WS-CONT-REGS
           .
      ******************************************************************10650099
       3100-LEER-ENTRADA-MOVS.                                          10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '3100-LEER-ENTRADA-MOVS'
      *F*
           INITIALIZE R350T077
      *

           READ ENTRADA-MOVS INTO R350T077                              10740099
             AT END                                                     10750099
                SET SW-SI-FINAL-MOVS TO TRUE                            10760099
                MOVE HIGH-VALUES TO WS-CLAVE1
                MOVE HIGH-VALUES TO WS-CLAVE3
             NOT AT END                                                 10770099

      *D*
      *      DISPLAY 'R350T077-KPOLIZA ' R350T077-KPOLIZA
      *      DISPLAY 'R350T077-KSUBPOLIZA ' R350T077-KSUBPOLIZA
      *      DISPLAY 'R350T077-DATOS-CUENTA(37:8) '
      *               R350T077-DATOS-CUENTA(37:8)
      *      DISPLAY 'R350T077-DATOS-CUENTA(29:8) '
      *               R350T077-DATOS-CUENTA(29:8)
      *F*
FECHAS*         IF R350T077-DATOS-CUENTA(37:8) > R350T000-FFINAL AND
FECHAS*            R350T077-KCONCEPTO = '0205'
FECHAS          IF (R350T077-DATOS-CUENTA(42:8) > R350T000-FFINAL AND
FECHAS             R350T077-KCONCEPTO = '0205') OR
FECHAS             R350T077-CTIPOREGMOV (1:2) = 'AP'
                   MOVE '0902' TO R350T077-KCONCEPTO
                END-IF

                MOVE R350T077-KPOLIZA(4:7) TO WS-POLIZA-1
                MOVE R350T077-KSUBPOLIZA   TO WS-SUBPOL-1
                MOVE R350T077-KPOLIZA(4:7) TO WS-POLIZA-3
                MOVE R350T077-KSUBPOLIZA   TO WS-SUBPOL-3

           END-READ.                                                    10880099
      *                                                                 10890099
      ******************************************************************10650099
       3200-LEER-ENTRADA-CTEC.                                          10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '3200-LEER-ENTRADA-CTEC'
      *F*

SINUSO*MF001 INITIALIZE DATOS-CTEC
MF001      INITIALIZE
                      X350JXX2
                      WS-FECHA-DESDE-NR
                      WS-FECHA-HASTA-NR

           MOVE WS-SUSCRI-AUX TO WS-SUSCRI-ANT
      *
           READ ENTRADA-CTEC INTO X350JXX2                              10740099
             AT END                                                     10750099
                SET SW-SI-FINAL-CTEC TO TRUE                            10760099
                MOVE HIGH-VALUES TO WS-CLAVE2
                MOVE HIGH-VALUES TO WS-CLAVE4
             NOT AT END                                                 10770099
                MOVE X350JXX2-FRENO-DESDE TO WS-FECHA-DESDE-ALFA-LTA
                MOVE X350JXX2-FRENO-HASTA TO WS-FECHA-HASTA-ALFA-LTA

                IF X350JXX2-FRENO-DESDE > R350T000-FFINAL
                   MOVE X350JXX2-FRENO-DESDE TO WS-FECHA-DESDE
                   COMPUTE WS-ANNO-DESDE = WS-ANNO-DESDE - 1
                   MOVE WS-FECHA-DESDE       TO X350JXX2-FRENO-DESDE
                END-IF

                MOVE X350JXX2-KPOLIZA  TO WS-POLIZA-2
                MOVE X350JXX2-KSUBPOL  TO WS-SUBPOL-2
                MOVE X350JXX2-KPOLIZA  TO WS-POLIZA-4
                MOVE X350JXX2-KSUBPOL  TO WS-SUBPOL-4
                MOVE X350JXX2-FRENO-DESDE TO WS-FECHA-DESDE-NR
                MOVE X350JXX2-FRENO-HASTA TO WS-FECHA-HASTA-NR

           END-READ.                                                    10880099
      *                                                                 10890099
      ******************************************************************10650099
       2703-LEER-TOTALES-ASEGURADO.                                     10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2703-LEER-TOTALES-ASEGURADO'
      *F*

           READ ENTRADA-TASE INTO X350JPX4                              10740099
             AT END                                                     10750099
                SET SW-SI-FINAL-TASE TO TRUE                            10760099
                MOVE HIGH-VALUES TO WS-CLAVE5
             NOT AT END                                                 10770099
                CONTINUE
           END-READ.                                                    10880099
      *                                                                 10890099
      ******************************************************************10650099
       2703-LEER-TOTALES-ASEGURADO-NE.                                  10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2703-LEER-TOTALES-ASEGURADO-NE'
      *F*

           READ ENTRADA-TASE-NE INTO X350JPX4                           10740099
             AT END                                                     10750099
                SET SW-SI-FINAL-TASE-NE TO TRUE                         10760099
                MOVE HIGH-VALUES TO WS-CLAVE5
             NOT AT END                                                 10770099
                CONTINUE
           END-READ.                                                    10880099
      *                                                                 10890099
      ******************************************************************10650099
       2703-LEER-TOTALES-ASEGURADO-CP.                                  10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2703-LEER-TOTALES-ASEGURADO-CP'
      *F*

           READ ENTRADA-TASE-CP INTO X350JPX5                           10740099
             AT END                                                     10750099
                SET SW-SI-FINAL-TASE-CP TO TRUE                         10760099
                MOVE HIGH-VALUES TO WS-CLAVE5
             NOT AT END                                                 10770099
                CONTINUE
           END-READ.                                                    10880099
      *                                                                 10890099
LTA   ******************************************************************10650099
       2703-LEER-TOTALES-LTA.                                           10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2703-LEER-TOTALES-LTA'
      *F*

           READ ENTRADA-TASE-LTA INTO X350JPX5                          10740099
             AT END                                                     10750099
                SET SW-SI-FINAL-TASE-LTA TO TRUE                        10760099
                MOVE HIGH-VALUES TO WS-CLAVE6
             NOT AT END                                                 10770099
                CONTINUE
           END-READ.                                                    10880099
      *                                                                 10890099
LTA   ******************************************************************10650099
       2703-LEER-TOTALES-ANTERIOR.                                      10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2703-LEER-TOTALES-ANTERIOR'
      *F*

           READ ENTRADA-TASE-ANT INTO X350JPX5                          10740099
             AT END                                                     10750099
                SET SW-SI-FINAL-TASE-ANT TO TRUE                        10760099
                MOVE HIGH-VALUES TO WS-CLAVE6
             NOT AT END                                                 10770099
                CONTINUE
           END-READ.                                                    10880099
      *                                                                 10890099
LTA   ******************************************************************10650099
       2703-LEER-TOTALES-LTP.                                           10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2703-LEER-TOTALES-LTP'
      *F*

           READ ENTRADA-TASE-LTP INTO X350JPX5                          10740099
             AT END                                                     10750099
                SET SW-SI-FINAL-TASE-LTP TO TRUE                        10760099
                MOVE HIGH-VALUES TO WS-CLAVE6
             NOT AT END                                                 10770099
                CONTINUE
           END-READ.                                                    10880099
      *                                                                 10890099
LTA   ******************************************************************10650099
       2703-LEER-TOTALES-LTB.                                           10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2703-LEER-TOTALES-LTB'
      *F*

           READ ENTRADA-TASE-LTB INTO X350JPX5                          10740099
             AT END                                                     10750099
                SET SW-SI-FINAL-TASE-LTB TO TRUE                        10760099
                MOVE HIGH-VALUES TO WS-CLAVE6
             NOT AT END                                                 10770099
                CONTINUE
           END-READ.                                                    10880099
      *                                                                 10890099
      ******************************************************************10650099
FICOL  2799-LEER-FICOL.                                                 10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2799-LEER-FICOL'
      *F*

           READ ENTRADA-FICOL INTO X350FICO                             10740099
             AT END                                                     10750099
                SET SW-SI-FINAL-FICOL TO TRUE                           10760099
      *D*
      *    DISPLAY 'TERMINA FICOL  '
      *F*

                MOVE HIGH-VALUES TO WS-CLAVE-FICOL
             NOT AT END                                                 10770099
      *D*
      *    DISPLAY 'X350JXX2-KSG-POLSUB' X350JXX2-KSG-POLSUB
      *    DISPLAY 'X350FICO-KPOLIZA   ' X350FICO-KPOLIZA
      *    DISPLAY 'X350FICO-BAREMO          ' X350FICO-BAREMO
      *    DISPLAY 'X350FICO-REAJUS-PB       ' X350FICO-REAJUS-PB
      *    DISPLAY 'X350FICO-GASTOS          ' X350FICO-GASTOS
      *    DISPLAY 'X350FICO-BONIFICACION    ' X350FICO-BONIFICACION
      *    DISPLAY 'X350FICO-REAJUS-TIPO     ' X350FICO-REAJUS-TIPO
      *    DISPLAY 'X350FICO-ANOS-PERDIDAS   ' X350FICO-ANOS-PERDIDAS
      *    DISPLAY 'X350FICO-ANOS-BENEFICIOS ' X350FICO-ANOS-BENEFICIOS
      *F*
                CONTINUE
           END-READ.                                                    10880099
      *                                                                 10890099
PBTEC ******************************************************************10650099
PBTEC  2704-LEER-FECHA.                                                 10660099
PBTEC ******************************************************************10670099
PBTEC *D*
PBTEC *    DISPLAY '2704-LEER-FECHA'
PBTEC *F*
PBTEC      INITIALIZE R350T000
PBTEC      SET SI-ENTRADA-FECHAS TO TRUE.
PBTEC
PBTEC      READ ENTRADA-FECHA INTO R350T000
PBTEC        AT END
PBTEC           SET FIN-ENTRADA-FECHAS TO TRUE
PBTEC      END-READ.
PBTEC *                                                                 10890099
      ******************************************************************10670099
PBTEC  3300-RECUPERAR-ALFA.
      ******************************************************************10670099
      * LLAMAMOS AL MODULO B310N505 PARA RECUPERAR EL VALOR DE ALFA
      *D*
      *    DISPLAY '3300-RECUPERAR-ALFA'
      *F*

           INITIALIZE WS-X310N505
                      WS-FEFECTO
                      WS-FCALCULO
                      WS-FORFAIT
                      WS-FPROXRENOV.

           MOVE X350JPX3-FRENO-DESDE     TO WS-FEFECTO
PBTEC      MOVE R350T000-FFINAL          TO WS-FCALCULO
           MOVE X350JPX3-FRENO-AHASTA    TO WS-FPROXRENOV

           MOVE X350JPX3-CTIPOALFA           TO WS-FORFAIT


           COMPUTE WS-NUM-FAEFECTO     = WS-NUM-FAEFECTO + WS-TC
           MOVE WS-NUM-FAEFECTO       TO WS-ANNO
           MOVE X350JPX3-FRENO-ADESDE    TO WS-FAEFECTO

           COMPUTE WS-NUM-FAPROXRENOV  = WS-NUM-FAPROXRENOV + WS-TC + 1
           MOVE WS-NUM-FAPROXRENOV    TO WS-ANNO
           MOVE X350JPX3-FRENO-AHASTA TO WS-FAPROXRENOV

           MOVE WS-FEFECTO               TO X310N505-FEFECTO-E
           MOVE WS-FCALCULO              TO X310N505-FCALCULO-E
           MOVE WS-FPROXRENOV            TO X310N505-FPRENOVA-E
           MOVE WS-FORFAIT               TO X310N505-FORFAIT-E

           CALL B310N505 USING WS-X310N505

           IF X310N505-CRETORNO-S NOT = ZEROS
              MOVE 'ERROR AL RECUPERAR ALFA NIIF17'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'B310N505'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE X310N505-CRETORNO-S TO X350J000-ID-ERROR-MOD

              PERFORM 2400-ESCRIBIR-ERRORES

           ELSE
              MOVE X310N505-ALFA-S      TO WS-ALFA
           END-IF

           IF WS-ALFA   > 1
              MOVE 1 TO WS-ALFA
           END-IF.

      ******************************************************************10670099
PBTEC  3300-RECUPERAR-ALFA-NR.
      ******************************************************************10670099
      * LLAMAMOS AL MODULO B310N505 PARA RECUPERAR EL VALOR DE ALFA
      *D*
      *    DISPLAY '3300-RECUPERAR-ALFA-NR'
      *F*

           INITIALIZE WS-X310N505
                      WS-FEFECTO
                      WS-FCALCULO
                      WS-FORFAIT
                      WS-FPROXRENOV.

           MOVE RSAL-FRENO-ADESDE        TO WS-FAEFECTO
           MOVE RSAL-FRENO-MDESDE        TO WS-FMEFECTO
           MOVE RSAL-FRENO-DDESDE        TO WS-FDEFECTO
PBTEC      MOVE R350T000-FFINAL          TO WS-FCALCULO
           MOVE RSAL-FRENO-AHASTA        TO WS-FAPROXRENOV
           MOVE RSAL-FRENO-MHASTA        TO WS-FMPROXRENOV
           MOVE RSAL-FRENO-DHASTA        TO WS-FDPROXRENOV

           MOVE X350JPX3-CTIPOALFA           TO WS-FORFAIT


           COMPUTE WS-NUM-FAEFECTO     = WS-NUM-FAEFECTO + WS-TC
           MOVE WS-NUM-FAEFECTO       TO WS-ANNO
           MOVE RSAL-FRENO-ADESDE        TO WS-FAEFECTO

           COMPUTE WS-NUM-FAPROXRENOV  = WS-NUM-FAPROXRENOV + WS-TC + 1
           MOVE WS-NUM-FAPROXRENOV    TO WS-ANNO
           MOVE RSAL-FRENO-AHASTA     TO WS-FAPROXRENOV

           MOVE WS-FEFECTO               TO X310N505-FEFECTO-E
           MOVE WS-FCALCULO              TO X310N505-FCALCULO-E
           MOVE WS-FPROXRENOV            TO X310N505-FPRENOVA-E
           MOVE WS-FORFAIT               TO X310N505-FORFAIT-E

           CALL B310N505 USING WS-X310N505

           IF X310N505-CRETORNO-S NOT = ZEROS
              MOVE 'ERROR AL RECUPERAR ALFA NIIF17'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'B310N505'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE X310N505-CRETORNO-S TO X350J000-ID-ERROR-MOD

              PERFORM 2400-ESCRIBIR-ERRORES

           ELSE
              MOVE X310N505-ALFA-S      TO WS-ALFA
           END-IF

           IF WS-ALFA   > 1
              MOVE 1 TO WS-ALFA
           END-IF.

      ******************************************************************10670099
PBTEC  3301-RECUPERAR-ALFA-MVT.
      ******************************************************************10670099
      * LLAMAMOS AL MODULO B310N505 PARA RECUPERAR EL VALOR DE ALFA
      *D*
      *    DISPLAY '3301-RECUPERAR-ALFA-MVT'
      *F*

           INITIALIZE WS-X310N505
                      WS-FEFECTO
                      WS-FCALCULO
                      WS-FORFAIT
                      WS-FPROXRENOV.

           MOVE X350JXX2-FRENO-DESDE     TO WS-FEFECTO
PBTEC      MOVE R350T000-FFINAL          TO WS-FCALCULO
           MOVE X350JXX2-FRENO-HASTA     TO WS-FPROXRENOV

           MOVE X350JXX2-CTIPOALFA           TO WS-FORFAIT


           COMPUTE WS-NUM-FAEFECTO     = WS-NUM-FAEFECTO + WS-TC
           MOVE WS-NUM-FAEFECTO       TO WS-ANNO
           MOVE X350JXX2-FRENO-ADESDE    TO WS-FAEFECTO

           COMPUTE WS-NUM-FAPROXRENOV  = WS-NUM-FAPROXRENOV + WS-TC + 1
           MOVE WS-NUM-FAPROXRENOV    TO WS-ANNO
           MOVE X350JXX2-FRENO-AHASTA TO WS-FAPROXRENOV

           MOVE WS-FEFECTO               TO X310N505-FEFECTO-E
           MOVE WS-FCALCULO              TO X310N505-FCALCULO-E
           MOVE WS-FPROXRENOV            TO X310N505-FPRENOVA-E
           MOVE WS-FORFAIT               TO X310N505-FORFAIT-E

           CALL B310N505 USING WS-X310N505

           IF X310N505-CRETORNO-S NOT = ZEROS
              MOVE 'ERROR AL RECUPERAR ALFA NIIF17'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'B310N505'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE X310N505-CRETORNO-S TO X350J000-ID-ERROR-MOD

              PERFORM 2400-ESCRIBIR-ERRORES

           ELSE
              MOVE X310N505-ALFA-S      TO WS-ALFA
           END-IF
      *D*
      *    DISPLAY 'X310N505-FEFECTO-E  ' X310N505-FEFECTO-E
      *    DISPLAY 'X310N505-FCALCULO-E ' X310N505-FCALCULO-E
      *    DISPLAY 'X310N505-FPRENOVA-E ' X310N505-FPRENOVA-E
      *    DISPLAY 'X310N505-FORFAIT-E  ' X310N505-FORFAIT-E
      *    DISPLAY 'X310N505-ALFA-S     ' X310N505-ALFA-S
      *    DISPLAY 'X310N505-CRETORNO-S ' X310N505-CRETORNO-S
      *    DISPLAY 'WS-FECHA-DESDE-ALFA-LTA ' WS-FECHA-DESDE-ALFA-LTA
      *    DISPLAY 'WS-ALFA   ' WS-ALFA
      *F*

           IF WS-ALFA   > 1
              MOVE 1 TO WS-ALFA
           END-IF.

      ******************************************************************10670099
LTA    3301-RECUPERAR-ALFA-MVT-LTP.
      ******************************************************************10670099
      * LLAMAMOS AL MODULO B310N505 PARA RECUPERAR EL VALOR DE ALFA

      *D*
      *    DISPLAY '3301-RECUPERAR-ALFA-MVT-LTP'
      *    DISPLAY 'WS-FECHA-DESDE-ALFA-LTA ' WS-FECHA-DESDE-ALFA-LTA
      *    DISPLAY 'R350T000-FFINAL    ' R350T000-FFINAL
      *    DISPLAY 'WS-FECHA-HASTA-ALFA-LTA ' WS-FECHA-HASTA-ALFA-LTA
      *    DISPLAY 'X350JXX2-CTIPOALFA ' X350JXX2-CTIPOALFA
      *F*

           INITIALIZE WS-X310N505
                      WS-FEFECTO
                      WS-FCALCULO
                      WS-FORFAIT
                      WS-FPROXRENOV.

           MOVE WS-FEC-AUX-LTP-D         TO WS-FEFECTO
           MOVE R350T000-FFINAL          TO WS-FCALCULO
           MOVE WS-FEC-AUX-LTP-H         TO WS-FPROXRENOV
           MOVE X350JXX2-CTIPOALFA       TO WS-FORFAIT

           COMPUTE WS-NUM-FAEFECTO     = WS-NUM-FAEFECTO + WS-TC
           MOVE WS-NUM-FAEFECTO       TO WS-ANNO
           MOVE WS-FEC-AUX-LTP-DA        TO WS-FAEFECTO

           COMPUTE WS-NUM-FAPROXRENOV  = WS-NUM-FAPROXRENOV + WS-TC + 1
           MOVE WS-NUM-FAPROXRENOV    TO WS-ANNO
           MOVE WS-FEC-AUX-LTP-HA     TO WS-FAPROXRENOV
      *D*
      *    DISPLAY 'WS-NUM-FAPROXRENOV ' WS-NUM-FAPROXRENOV
      *    DISPLAY 'WS-FAPROXRENOV     ' WS-FAPROXRENOV
      *F*
JOSEN      INITIALIZE WS-FECHA-DESDE-ALFA-LTA
JOSEN                 WS-FECHA-HASTA-ALFA-LTA

           IF WS-FECHA-DESDE-ALFA-LTA EQUAL ZEROES
              MOVE X350JXX2-FRENO-DESDE TO WS-FECHA-DESDE-ALFA-LTA
           END-IF

           IF WS-FECHA-HASTA-ALFA-LTA EQUAL ZEROES
              MOVE X350JXX2-FRENO-HASTA TO WS-FECHA-HASTA-ALFA-LTA
           END-IF

JOSEN           IF WS-ANNO-DESDE-LTA-H    < R350T000-FAFINAL AND
1002               WS-RENOVADA-LTA EQUAL 'N'
                   ADD  1                      TO WS-ANNO-DESDE-LTA-H
JOSEN           END-IF

0124       IF SW-SI-LTA-FINAL AND SW-SW-LTPA EQUAL 'N'
                              AND WS-RENOVADA-LTA EQUAL 'N'

0124          MOVE WS-FEC-AUX-LTP-D     TO WS-FECHA-DESDE-ALFA-LTA
0124       END-IF

REVI       MOVE WS-FECHA-DESDE-ALFA-LTA TO WS-FEFECTO
REVI       MOVE WS-FECHA-HASTA-ALFA-LTA TO WS-FPROXRENOV
           MOVE WS-FEFECTO               TO X310N505-FEFECTO-E
           MOVE WS-FCALCULO              TO X310N505-FCALCULO-E
           MOVE WS-FPROXRENOV            TO X310N505-FPRENOVA-E
           MOVE WS-FORFAIT               TO X310N505-FORFAIT-E

      *D*
      *    DISPLAY 'WS-FEC-AUX-LTP-D-ANTES    ' WS-FEC-AUX-LTP-D
      *F*
RF001      IF WS-FEC-AUX-LTP-D EQUAL SPACES OR LOW-VALUES
RF001         MOVE WS-FECHA-DESDE-ALFA-LTA TO WS-FEC-AUX-LTP-D
RF001      END-IF

      *D*
      *    DISPLAY 'X310N505-FEFECTO-E-ANTES  ' X310N505-FEFECTO-E
      *    DISPLAY 'WS-FEC-AUX-LTP-D-DESPUES ' WS-FEC-AUX-LTP-D
      *F*
1002          IF X310N505-FEFECTO-E > WS-FEC-AUX-LTP-D
1002            MOVE WS-FEC-AUX-LTP-D     TO X310N505-FEFECTO-E
1002          END-IF

      *D*
      *    DISPLAY 'X310N505-FEFECTO-E-DESPUES2 ' X310N505-FEFECTO-E
      *    DISPLAY 'WS-FEC-AUX-LTP-D-DESPUES2   ' WS-FEC-AUX-LTP-D
      *F*


      *D*
      *    DISPLAY 'LLAMADA A RUTINA B310N505'
      *F*
           CALL B310N505 USING WS-X310N505
      *D*
      *    DISPLAY 'X310N505-FEFECTO-E  ' X310N505-FEFECTO-E
      *    DISPLAY 'X310N505-FCALCULO-E ' X310N505-FCALCULO-E
      *    DISPLAY 'X310N505-FPRENOVA-E ' X310N505-FPRENOVA-E
      *    DISPLAY 'X310N505-FORFAIT-E  ' X310N505-FORFAIT-E
      *    DISPLAY 'X310N505-ALFA-S     ' X310N505-ALFA-S
      *    DISPLAY 'X310N505-CRETORNO-S ' X310N505-CRETORNO-S
      *    DISPLAY 'WS-FECHA-DESDE-ALFA-LTA ' WS-FECHA-DESDE-ALFA-LTA
      *    DISPLAY 'WS-ALFA   ' WS-ALFA
      *F*

           INITIALIZE WS-FECHA-DESDE-ALFA-LTA
                      WS-FECHA-HASTA-ALFA-LTA
      *D*
      *    DISPLAY 'X310N505-CRETORNO-S ' X310N505-CRETORNO-S
      *F*
           IF X310N505-CRETORNO-S NOT = ZEROS
              CONTINUE
           ELSE
              MOVE X310N505-ALFA-S      TO WS-ALFA
           END-IF

      *D*
      *    DISPLAY 'WS-ALFA   ' WS-ALFA
      *    DISPLAY 'WS-FEC-AUX-LTP-H ' WS-FEC-AUX-LTP-H
      *    DISPLAY 'WS-FEC-AUX-LTP-D ' WS-FEC-AUX-LTP-D
      *    DISPLAY 'X350JXX2-FRENO-DESDE ' X350JXX2-FRENO-DESDE
      *    DISPLAY 'X350JXX2-FRENO-HASTA ' X350JXX2-FRENO-HASTA
      *    DISPLAY 'WS-FECHA-DESDE-ALFA-LTA ' WS-FECHA-DESDE-ALFA-LTA
      *    DISPLAY 'WS-FECHA-HASTA-ALFA-LTA ' WS-FECHA-HASTA-ALFA-LTA
      *    DISPLAY 'WS-FCALCULO ' WS-FCALCULO
      *    DISPLAY 'WS-FEFECTO  ' WS-FEFECTO
      *    DISPLAY 'WS-FPROXRENOV ' WS-FPROXRENOV
      *    DISPLAY 'WS-FCALCULO ' WS-FCALCULO
      *    DISPLAY 'WS-FORFAIT  ' WS-FORFAIT
      *F*

           IF WS-ALFA   > 1
              MOVE 1 TO WS-ALFA
           END-IF.
      ******************************************************************10670099
LTA    3301-RECUPERAR-ALFA-MVT-LTB.
      ******************************************************************10670099
      * LLAMAMOS AL MODULO B310N505 PARA RECUPERAR EL VALOR DE ALFA

      *D*
      *    DISPLAY '3301-RECUPERAR-ALFA-MVT-LTP'
      *    DISPLAY 'WS-FECHA-DESDE-ALFA-LTA ' WS-FECHA-DESDE-ALFA-LTA
      *    DISPLAY 'R350T000-FFINAL    ' R350T000-FFINAL
      *    DISPLAY 'WS-FECHA-HASTA-ALFA-LTA ' WS-FECHA-HASTA-ALFA-LTA
      *    DISPLAY 'X350JXX2-CTIPOALFA ' X350JXX2-CTIPOALFA
      *F*

           INITIALIZE WS-X310N505
                      WS-FEFECTO
                      WS-FCALCULO
                      WS-FORFAIT
                      WS-FPROXRENOV.


           MOVE WS-FEC-AUX-LTP-D         TO WS-FEFECTO
           MOVE R350T000-FFINAL          TO WS-FCALCULO
           MOVE WS-FEC-AUX-LTP-H         TO WS-FPROXRENOV
           MOVE X350JPX3-CTIPOALFA       TO WS-FORFAIT

           COMPUTE WS-NUM-FAEFECTO     = WS-NUM-FAEFECTO + WS-TC
           MOVE WS-NUM-FAEFECTO       TO WS-ANNO
           MOVE WS-FEC-AUX-LTP-DA        TO WS-FAEFECTO

           COMPUTE WS-NUM-FAPROXRENOV  = WS-NUM-FAPROXRENOV + WS-TC + 1
           MOVE WS-NUM-FAPROXRENOV    TO WS-ANNO
           MOVE WS-FEC-AUX-LTP-HA     TO WS-FAPROXRENOV
      *D*
      *    DISPLAY 'WS-NUM-FAPROXRENOV ' WS-NUM-FAPROXRENOV
      *    DISPLAY 'WS-FAPROXRENOV     ' WS-FAPROXRENOV
      *F*
JOSEN      INITIALIZE WS-FECHA-DESDE-ALFA-LTA
JOSEN                 WS-FECHA-HASTA-ALFA-LTA

           IF WS-FECHA-DESDE-ALFA-LTA EQUAL ZEROES
              MOVE X350JPX3-FRENO-DESDE TO WS-FECHA-DESDE-ALFA-LTA
           END-IF

           IF WS-FECHA-HASTA-ALFA-LTA EQUAL ZEROES
              MOVE X350JPX3-FRENO-HASTA TO WS-FECHA-HASTA-ALFA-LTA
           END-IF

JOSEN           IF WS-ANNO-DESDE-LTA-H    < R350T000-FAFINAL
                   ADD  1                      TO WS-ANNO-DESDE-LTA-H
JOSEN           END-IF

REVI       MOVE WS-FECHA-DESDE-ALFA-LTA TO WS-FEFECTO
REVI       MOVE WS-FECHA-HASTA-ALFA-LTA TO WS-FPROXRENOV

           MOVE WS-FEFECTO               TO X310N505-FEFECTO-E
           MOVE WS-FCALCULO              TO X310N505-FCALCULO-E
           MOVE WS-FPROXRENOV            TO X310N505-FPRENOVA-E
           MOVE WS-FORFAIT               TO X310N505-FORFAIT-E

      *D*
      *    DISPLAY 'LLAMADA A RUTINA B310N505'
      *F*
           CALL B310N505 USING WS-X310N505

           INITIALIZE WS-FECHA-DESDE-ALFA-LTA
                      WS-FECHA-HASTA-ALFA-LTA
      *D*
      *    DISPLAY 'X310N505-CRETORNO-S ' X310N505-CRETORNO-S
      *F*
           IF X310N505-CRETORNO-S NOT = ZEROS
              CONTINUE
           ELSE
              MOVE X310N505-ALFA-S      TO WS-ALFA
           END-IF

      *D*
      *    DISPLAY 'WS-ALFA   ' WS-ALFA
      *F*

           IF WS-ALFA   > 1
              MOVE 1 TO WS-ALFA
           END-IF.
LTA
      ******************************************************************10670099
       CALCULAR-ANNOS-TRANSCURRIDOS.
      ******************************************************************10670099
      *D*
      *    DISPLAY 'CALCULAR-ANNOS-TRANSCURRIDOS'
      *F*
           INITIALIZE WS-ENT-C536N002
           INITIALIZE WS-E-INCIDENCIAS
           INITIALIZE WS-SALIDA
           INITIALIZE WS-RETORNO
           INITIALIZE WS-TC WS-TC-I

           MOVE X350JPX3-FEFECTO-INIPOL       TO WS-FECHA-AUX
           MOVE WS-DIA-AUX                    TO WS-DIAFIN
           MOVE WS-MES-AUX                    TO WS-MESFIN
           MOVE WS-ANNO-AUX                   TO WS-ANNOFIN
           MOVE X350JPX3-FRENO-DDESDE         TO WS-DIAINICIO
           MOVE X350JPX3-FRENO-MDESDE         TO WS-MESINICIO
           MOVE X350JPX3-FRENO-ADESDE         TO WS-ANNOINICIO
           MOVE 1                             TO WS-CRITERIO
      *    SE REALIZA LA LLAMADA
           CALL C536N002 USING BY REFERENCE WS-ENT-C536N002
                                               WS-SALIDA
                                               WS-E-INCIDENCIAS

                                     RETURNING WS-RETORNO

           IF WS-RETORNO NOT = ZEROES
              MOVE 'ERROR AL RECUPERAR ANNO TRANSCURRIDO NIIF17'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'C536N002'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE WS-RETORNO          TO X350J000-ID-ERROR-MOD

              PERFORM 2400-ESCRIBIR-ERRORES

           ELSE
              MOVE WS-SALIDA  TO WS-TC-I
              MOVE WS-TC-I    TO WS-TC
              CONTINUE

           END-IF.

      ******************************************************************10670099
       CALCULAR-ANNOS-TRANS-MVT.
      ******************************************************************10670099
      *D*
      *    DISPLAY 'CALCULAR-ANNOS-TRANS-MVT'
      *F*
           INITIALIZE WS-ENT-C536N002
           INITIALIZE WS-E-INCIDENCIAS
           INITIALIZE WS-SALIDA
           INITIALIZE WS-RETORNO
           INITIALIZE WS-TC WS-TC-I
      *    SE MUEVEN LOS DATOS A LA ENTRADA
           MOVE X350JXX2-FEFECTO-INIPOL       TO WS-FECHA-AUX
           MOVE WS-DIA-AUX                    TO WS-DIAFIN
           MOVE WS-MES-AUX                    TO WS-MESFIN
           MOVE WS-ANNO-AUX                   TO WS-ANNOFIN
           MOVE X350JXX2-FRENO-DDESDE         TO WS-DIAINICIO
           MOVE X350JXX2-FRENO-MDESDE         TO WS-MESINICIO
           MOVE X350JXX2-FRENO-ADESDE         TO WS-ANNOINICIO
           MOVE 1                             TO WS-CRITERIO
      *    SE REALIZA LA LLAMADA
           CALL C536N002 USING BY REFERENCE WS-ENT-C536N002
                                               WS-SALIDA
                                               WS-E-INCIDENCIAS

                                     RETURNING WS-RETORNO
      *D*
      *    DISPLAY 'SALGO DE LLAMAR'
      *F*

           IF WS-RETORNO NOT = ZEROES
              MOVE 'ERROR AL ANNO TRANSCURRIDO NIIF17'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'C536N002'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE WS-RETORNO          TO X350J000-ID-ERROR-MOD

              PERFORM 2400-ESCRIBIR-ERRORES

           ELSE
              MOVE WS-SALIDA  TO WS-TC-I
              MOVE WS-TC-I    TO WS-TC
              CONTINUE

           END-IF.

      ******************************************************************10670099
LTA    CALCULAR-ANNOS-TRANS-MVT-LTP.
      ******************************************************************10670099
           INITIALIZE WS-ENT-C536N002
           INITIALIZE WS-E-INCIDENCIAS
           INITIALIZE WS-SALIDA
           INITIALIZE WS-RETORNO
           INITIALIZE WS-TC WS-TC-I

      *    SE MUEVEN LOS DATOS A LA ENTRADA
           MOVE X350JXX2-FEFECTO-INIPOL       TO WS-FECHA-AUX
           MOVE WS-DIA-AUX                    TO WS-DIAFIN
           MOVE WS-MES-AUX                    TO WS-MESFIN
           MOVE WS-ANNO-AUX                   TO WS-ANNOFIN
           MOVE WS-FEC-AUX-LTP-DD             TO WS-DIAINICIO
           MOVE WS-FEC-AUX-LTP-DM             TO WS-MESINICIO
           MOVE WS-FEC-AUX-LTP-DA             TO WS-ANNOINICIO
           MOVE 1                             TO WS-CRITERIO
      *D*
      *    DISPLAY 'CALCULAR-ANNOS-TRANS-MVT-LTP'
      *    DISPLAY 'X350JXX2-FEFECTO-INIPOL ' X350JXX2-FEFECTO-INIPOL
      *    DISPLAY 'WS-DIA-AUX        ' WS-DIA-AUX
      *    DISPLAY 'WS-MES-AUX        ' WS-MES-AUX
      *    DISPLAY 'WS-ANNO-AUX       ' WS-ANNO-AUX
      *    DISPLAY 'WS-FEC-AUX-LTP-DD ' WS-FEC-AUX-LTP-DD
      *    DISPLAY 'WS-FEC-AUX-LTP-DM ' WS-FEC-AUX-LTP-DM
      *    DISPLAY 'WS-FEC-AUX-LTP-DA ' WS-FEC-AUX-LTP-DA
      *F*
      *    SE REALIZA LA LLAMADA
           CALL C536N002 USING BY REFERENCE WS-ENT-C536N002
                                               WS-SALIDA
                                               WS-E-INCIDENCIAS

                                     RETURNING WS-RETORNO

           IF WS-RETORNO NOT = ZEROES
              CONTINUE
           ELSE
              MOVE WS-SALIDA  TO WS-TC-I
              MOVE WS-TC-I    TO WS-TC
      *D*
      *       DISPLAY 'WS-TC ' WS-TC
      *F*
              CONTINUE

           END-IF.
      ******************************************************************10670099
LTA    CALCULAR-ANNOS-TRANS-MVT-LTB.
      ******************************************************************10670099
           INITIALIZE WS-ENT-C536N002
           INITIALIZE WS-E-INCIDENCIAS
           INITIALIZE WS-SALIDA
           INITIALIZE WS-RETORNO
           INITIALIZE WS-TC WS-TC-I

      *    SE MUEVEN LOS DATOS A LA ENTRADA
           MOVE X350JPX3-FEFECTO-INIPOL       TO WS-FECHA-AUX
           MOVE WS-DIA-AUX                    TO WS-DIAFIN
           MOVE WS-MES-AUX                    TO WS-MESFIN
           MOVE WS-ANNO-AUX                   TO WS-ANNOFIN
           MOVE WS-FEC-AUX-LTP-DD             TO WS-DIAINICIO
           MOVE WS-FEC-AUX-LTP-DM             TO WS-MESINICIO
           MOVE WS-FEC-AUX-LTP-DA             TO WS-ANNOINICIO
           MOVE 1                             TO WS-CRITERIO
      *D*
      *    DISPLAY 'CALCULAR-ANNOS-TRANS-MVT-LTP'
      *    DISPLAY 'X350JXX2-FEFECTO-INIPOL ' X350JXX2-FEFECTO-INIPOL
      *    DISPLAY 'WS-DIA-AUX        ' WS-DIA-AUX
      *    DISPLAY 'WS-MES-AUX        ' WS-MES-AUX
      *    DISPLAY 'WS-ANNO-AUX       ' WS-ANNO-AUX
      *    DISPLAY 'WS-FEC-AUX-LTP-DD ' WS-FEC-AUX-LTP-DD
      *    DISPLAY 'WS-FEC-AUX-LTP-DM ' WS-FEC-AUX-LTP-DM
      *    DISPLAY 'WS-FEC-AUX-LTP-DA ' WS-FEC-AUX-LTP-DA
      *F*
      *    SE REALIZA LA LLAMADA
           CALL C536N002 USING BY REFERENCE WS-ENT-C536N002
                                               WS-SALIDA
                                               WS-E-INCIDENCIAS

                                     RETURNING WS-RETORNO

           IF WS-RETORNO NOT = ZEROES
              CONTINUE
           ELSE
              MOVE WS-SALIDA  TO WS-TC-I
              MOVE WS-TC-I    TO WS-TC
      *D*
      *       DISPLAY 'WS-TC ' WS-TC
      *F*
              CONTINUE

           END-IF.
LTA
      ******************************************************************10650099
       5000-GRABAR-SALIDA.                                              10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5000-GRABAR-SALIDA'
      *F*
RF001         IF WS-RENOVADA EQUAL "N" AND SW-SI-LTA-SOLO-ANT-P
RF001            CONTINUE
RF001         ELSE
                 WRITE REG-SALIDA1 FROM RSALIDA
RF001         END-IF

           MOVE WS-POLIZA-2 TO WS-POLIZA-2-ANT
           MOVE WS-SUBPOL-2 TO WS-SUBPOL-2-ANT
           MOVE WS-POLIZA-4 TO WS-POLIZA-4-ANT
           MOVE WS-SUBPOL-4 TO WS-SUBPOL-4-ANT

           INITIALIZE RSALIDA
           .
LTA   ******************************************************************10650099
       5000-GRABAR-SALIDA-LTP-1.                                        10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5000-GRABAR-SALIDA-LTP-1'
      *F*
           WRITE REG-SALIDA1 FROM RSALIDA

      *    MOVE WS-POLIZA-2 TO WS-POLIZA-2-ANT
      *    MOVE WS-SUBPOL-2 TO WS-SUBPOL-2-ANT
      *    MOVE WS-POLIZA-4 TO WS-POLIZA-4-ANT
      *    MOVE WS-SUBPOL-4 TO WS-SUBPOL-4-ANT

           INITIALIZE RSALIDA
LTA        .
LTA   ******************************************************************10650099
       5000-GRABAR-SALIDA-LTP-2.                                        10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5000-GRABAR-SALIDA-LTP-2'
      *    DISPLAY 'SW-SW-LTPA              ' SW-SW-LTPA
      *F*
SINUSO*MF001 INITIALIZE DATOS-CTEC
MF001      INITIALIZE
                      RSAL-PRIMA-RECIBOS
                      X350JPX6-PRIMA-RECIBOS
                      RSAL-APUNTE-RECIBO
                      X350JPX6-APUNTE-RECIBO
                      RSAL-TOTAL-SINIES
                      RSAL-INGRESOS-GASTOS
                      X350JPX6-INGRESOS-GASTOS
                      RSAL-NETA-COASEGUR
                      X350JPX6-NETA-COASEGUR
      *               RSAL-PB-TECNICA
                      X350JPX6-PB-TECNICA
                      RSAL-DEV-NETCOASE
                      X350JPX6-DEV-NETCOASE
                      WS-IMPORTE-REPARTOS

           MOVE ZEROES TO RSAL-PRIMA-RECIBOS
                          X350JPX6-PRIMA-RECIBOS

           MOVE '3000'                   TO RSAL-MODELO

           MOVE X350JXX2-CCANAL          TO RSAL-CANAL
                                            X350JPX6-CANAL
           MOVE X350JXX2-KPOLIZA         TO RSAL-POLIZA
           MOVE X350JXX2-KSUBPOL         TO RSAL-SUBPOLIZA
           MOVE X350JXX2-KSUSCRI         TO RSAL-SUSCRIP

           MOVE X350JXX2-FEFECTO-AINIPOL TO RSAL-FORIGENASUSCRI
           MOVE X350JXX2-FEFECTO-MINIPOL TO RSAL-FORIGENMSUSCRI
           MOVE X350JXX2-FEFECTO-DINIPOL TO RSAL-FORIGENDSUSCRI

           MOVE X350JXX2-KRAMO           TO RSAL-RAMO
                                            X350JPX6-RAMO
           MOVE X350JXX2-MODALI          TO RSAL-MODALIDAD
                                            X350JPX6-MODALIDAD
           MOVE X350JXX2-VIGOR-ANUL      TO RSAL-CESTADOASEG
           MOVE '00'                     TO RSAL-PREGRUPO(1:2)
           MOVE X350JXX2-CARTINV         TO RSAL-PREGRUPO(3:2)
           MOVE X350JXX2-CASADA          TO RSAL-CASADO
           MOVE X350JXX2-PINTER-TECBASE  TO RSAL-I1BTI

      *    MOVE WS-ANNO-DESDE-TRAMO      TO RSAL-FRENO-ADESDE
      *    MOVE WS-MES-DESDE-TRAMO       TO RSAL-FRENO-MDESDE
      *    MOVE WS-DIA-DESDE-TRAMO       TO RSAL-FRENO-DDESDE
PLURIN       MOVE WS-FRENO-DESDE-APLURI-ACTUAL TO RSAL-FRENO-ADESDE
PLURIN       MOVE WS-FRENO-DESDE-MPLURI-ACTUAL TO RSAL-FRENO-MDESDE
PLURIN       MOVE WS-FRENO-DESDE-DPLURI-ACTUAL TO RSAL-FRENO-DDESDE

           MOVE WS-ANNO-HASTA-TRAMO   TO RSAL-FRENO-AHASTA
           MOVE WS-MES-HASTA-TRAMO    TO RSAL-FRENO-MHASTA
           MOVE WS-DIA-HASTA-TRAMO    TO RSAL-FRENO-DHASTA

FECHAS*09/04/2024 - PARA IGUALAR CON B350JL03 - comentado con Jose
FECHAS*09/04/2024 -  fase1 de momento todos los PLURIS son renovados
FECHAS     MOVE WS-RENOVADA              TO RSAL-RENOVADA
FECHAS                                      X350JPX6-RENOVADA
fechas*    MOVE SPACES TO WS-RENOVADA
FECHAS*    IF WS-FECHA-LTA-H         >  R350T000-FFINAL
FECHAS*    OR WS-FECHA-LTA-H         =  R350T000-FFINAL
FECHAS*       MOVE 'S'                   TO RSAL-RENOVADA
FECHAS*                                     X350JPX6-RENOVADA
FECHAS*       MOVE 'S'                   TO WS-RENOVADA
FECHAS*    ELSE
FECHAS*       MOVE 'N'                   TO RSAL-RENOVADA
FECHAS*                                     X350JPX6-RENOVADA
FECHAS*       MOVE 'N' TO WS-RENOVADA
FECHAS*    END-IF

JOSEN           IF WS-FECHA-HASTA-TRAMO < R350T000-FFINAL
JOSEN              MOVE WS-ANNO-HASTA-TRAMO TO RSAL-FCIERREA
                                                 X350JPX6-FCIERREA
JOSEN              MOVE WS-MES-HASTA-TRAMO  TO RSAL-FCIERREM
                                                 X350JPX6-FCIERREM
JOSEN              MOVE WS-DIA-HASTA-TRAMO  TO RSAL-FCIERRED
                                                 X350JPX6-FCIERRED
JOSEN           ELSE
                   MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
                                                 X350JPX6-FCIERREA
                   MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
                                                 X350JPX6-FCIERREM
                   MOVE R350T000-FDFINAL         TO RSAL-FCIERRED
                                                 X350JPX6-FCIERRED
JOSEN           END-IF

      *    MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
      *                                     X350JPX6-FCIERREA
      *    MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
      *                                     X350JPX6-FCIERREM
      *    MOVE R350T000-FDFINAL         TO RSAL-FCIERRED
      *                                     X350JPX6-FCIERRED
           MOVE 'PLURI'                  TO RSAL-LTA
           MOVE X350JXX2-FCOBRO          TO RSAL-FORMAPAGO
           MOVE X350JXX2-TIPO-COASE      TO RSAL-COASEGURO
           MOVE X350JXX2-PCOASE          TO RSAL-PCOASEGURO
           MOVE X350JXX2-PAGADOR         TO RSAL-PGLOBAL

           IF X350JXX2-CERTI EQUAL '000000' AND                         000
              X350JXX2-ASEG-MECANIZADOS = 'N'                           000
              MOVE 'S' TO RSAL-IMNOMINADA                               000
           ELSE
              MOVE 'N' TO RSAL-IMNOMINADA                               000
           END-IF

           MOVE X350JXX2-NASEGURADO      TO RSAL-NASEGURADOS

           IF X350JXX2-VIGOR-ANUL  = 'A'
              MOVE X350JXX2-ANO-ANUL     TO RSAL-FECAANULACION
              MOVE X350JXX2-MES-ANUL     TO RSAL-FECMANULACION
              MOVE X350JXX2-DIA-ANUL     TO RSAL-FECDANULACION
              MOVE X350JXX2-MOTIVO-ANU   TO RSAL-MOTIVO
           END-IF

           MOVE WS-ALFA                  TO RSAL-ALFA
      *D*
      *    DISPLAY 'WS-ALFA-PLURI-2  ' WS-ALFA
      *F*

EMPMAR**   MOVE X350JXX2-KGRUEMP         TO RSAL-GEMPRESA

130624     MOVE WS-KGRUPEMP-PS-ACTUAL TO X350M004-KGRUPEMP-PS
EMPMAR     IF X350M004-KGRUPEMP-PS = ZEROS
EMPMAR        MOVE SPACES                TO X350M004-KGRUPEMP-PS
EMPMAR     END-IF

EMPMAR     MOVE X350M004-KGRUPEMP-PS(3:3) TO RSAL-GEMPRESA

PBTECJ*    IF AUX-TIP-REAJUSTE = '1' OR 'T'
      *       MOVE 'S'                      TO RSAL-REAJUSTE
      *    ELSE
      *       MOVE 'N'                      TO RSAL-REAJUSTE
      *    END-IF

           MOVE AUX-CCOBON-REAJUS        TO RSAL-REAJUSTE

           MOVE AUX-REAJUSTE-PB          TO RSAL-REAJUSTE-PB

           MOVE WS-REAJREC               TO RSAL-REAJUSTE-REC
           MOVE AUX-TIP-REAJUSTE         TO RSAL-TIP-REAJUSTE
           MOVE AUX-NANIOPER             TO RSAL-APERDIDAS
           MOVE AUX-NANIOBEN             TO RSAL-ABENEFICI

           IF X350X098-IANTICIPO-S > ZEROES
              MOVE 'S'                   TO RSAL-ANTICIPO-PB
           ELSE
              MOVE 'N'                   TO RSAL-ANTICIPO-PB
           END-IF

           MOVE AUX-PANTICIPO-S          TO RSAL-P-ANTICIPO
           MOVE AUX-CCUADROBAREMO-S      TO RSAL-BAREMO
           MOVE AUX-PPORCEN-REAL         TO RSAL-REAJUSTEO
           MOVE AUX-PCALCULOPB-S         TO RSAL-P-REAUSTE-PB
210524*    MOVE X350M004-PREAJPB-PS      TO RSAL-P-PB-APLICAR
210524*    MOVE X350M004-PREAJPB-PS      TO RSAL-P-REAUSTE-PB
210524*    MOVE X350M004-PREAJPB-PS      TO AUX-PCALCULOPB-S
210524*    MOVE X350M004-PREAJPB-PS      TO RSAL-P-GASTOS-PB
210524     MOVE WS-PRREAPB-PS-ACTUAL     TO RSAL-P-PB-APLICAR
210524     MOVE WS-PRREAPB-PS-ACTUAL     TO RSAL-P-REAUSTE-PB
210524     MOVE WS-PRREAPB-PS-ACTUAL     TO AUX-PCALCULOPB-S
210524     MOVE WS-PGASTOS-PS-ACTUAL     TO RSAL-P-GASTOS-PB

      *    IF AUX-PPORCEN-REAL    = ZEROES
      *       MOVE AUX-PCALCULOPB-S      TO RSAL-P-PB-APLICAR
      *       IF AUX-CCUADROBAREMO-S EQUAL ZEROES
      *       MOVE SPACES                TO RSAL-BAREMO
      *       END-IF
      *       IF AUX-PCALCULOPB-S = ZEROES
      *          MOVE ZEROES TO RSAL-P-PB-APLICAR
      *          IF AUX-CCUADROBAREMO-S EQUAL ZEROES
      *          MOVE SPACES              TO RSAL-BAREMO
      *          END-IF
      *       END-IF
      *    ELSE
      *       MOVE AUX-PPORCEN-REAL      TO RSAL-P-PB-APLICAR
      *
      *    END-IF

      *    IF AUX-PPORCEN-REAL NOT = ZEROES AND
      *       AUX-PCALCULOPB-S NOT = ZEROES
      *       MOVE AUX-CCUADROBAREMO-S TO RSAL-BAREMO
      *       MOVE AUX-PCALCULOPB-S    TO RSAL-P-PB-APLICAR
      *    END-IF


      *D*
      *    DISPLAY 'WS-IMPORTE-PRI ' WS-IMPORTE-PRI
      *F*
      *    COMPUTE RSAL-PRIMA-ANUAL ROUNDED =  WS-IMPORTE-PRI
josen      COMPUTE RSAL-PRIMA-ANUAL ROUNDED = X350JXX2-EPNETA

           MOVE WS-IMPORTE-PRI           TO X350JPX6-PRIMA-ANUAL
           MOVE ZEROES                   TO RSAL-GASTOS
                                            X350JPX6-GASTOS
           MOVE X350X098-IARRASTRE-S TO RSAL-IMP-ARRAS-PERD
           MOVE ZEROES                   TO RSAL-NETA-COASEGUR
                                            X350JPX6-NETA-COASEGUR

           MOVE SPACES TO X350JPX5-GEMPRESA

EMPMAR**   IF X350JXX2-KGRUEMP = '000'
EMPMAR     IF X350M004-KGRUPEMP-PS = SPACES
EMPMAR        MOVE SPACES           TO X350JPX3-KGRUEMP
           END-IF

EMPMAR**   IF X350JXX2-KGRUEMP NOT = SPACES
EMPMAR     IF X350M004-KGRUPEMP-PS NOT = SPACES
             MOVE X350JPX4-NASEGURADO    TO RSAL-ASE-TOT-EMPRESA
           END-IF

      *D*
      *    DISPLAY 'X350JXX2-KGRUEMP ' X350JXX2-KGRUEMP
      *    DISPLAY 'X350M004-KGRUPEMP-PS(3:3) '
      *             X350M004-KGRUPEMP-PS(3:3)
      *F*
0124       INITIALIZE X350JPX5

EMPMAR**   IF X350JXX2-KGRUEMP NOT = SPACES
EMPMAR     IF X350M004-KGRUPEMP-PS NOT = SPACES
              OPEN INPUT ENTRADA-TASE-LTP
              PERFORM 2703-LEER-TOTALES-LTP
EMPMAR**        UNTIL X350JXX2-KGRUEMP EQUAL
EMPMAR          UNTIL X350M004-KGRUPEMP-PS(3:3) EQUAL
                      X350JPX5-GEMPRESA
                CLOSE ENTRADA-TASE-LTP

      *       OPEN INPUT ENTRADA-TASE-CP
      *          PERFORM 2703-LEER-TOTALES-ASEGURADO-CP
      *            UNTIL X350JPX3-KPOLIZA EQUAL
      *                  X350JPX5-KPOLIZA AND
      *                  X350JPX3-KSUBPOL EQUAL
      *                  X350JPX5-KSUBPOL
      *            CLOSE ENTRADA-TASE-CP

              IF X350JPX5-PRIMA-ANUAL <= ZEROES
                 MOVE ZEROES TO WS-IMPORTE-REPARTOS
              ELSE
PBTEC *          COMPUTE WS-IMPORTE-REPARTOS = ((WS-IMPORTE-PERIOD -
PBTEC *                                        WS-IMPORTE-206     -
PBTEC *                                        WS-IMP-GASTOS)   *
PBTEC *                                        X350JPX5-PRIMA-ANUAL) /
PBTEC *                                        X350JPX4-PRIMA-ANUAL
      *D*
      *    DISPLAY 'IMPORTE-REPARTOS      '
      *    DISPLAY 'X350JXX2-KPOLIZA      ' X350JXX2-KPOLIZA
      *    DISPLAY 'X350JXX2-KSUBPOL      ' X350JXX2-KSUBPOL
      *    DISPLAY 'X350JPX5-PRIMA-PERIOD ' X350JPX5-PRIMA-PERIOD
      *    DISPLAY 'X350JPX5-TOTAL-SINIES ' X350JPX5-TOTAL-SINIES
      *    DISPLAY 'X350JPX5-GASTOS       ' X350JPX5-GASTOS
      *    DISPLAY 'X350JPX5-PBPAGADA-EMP ' X350JPX5-PBPAGADA-EMP
      *    DISPLAY 'WS-IMPORTE-PRI        ' WS-IMPORTE-PRI
      *    DISPLAY 'X350JXX2-EPNETA       ' X350JXX2-EPNETA
      *    DISPLAY 'X350JPX5-PRIMA-ANUAL  ' X350JPX5-PRIMA-ANUAL
      *F*
LTA              COMPUTE WS-IMPORTE-REPARTOS =
LTA                                   ((X350JPX5-PRIMA-PERIOD    -
LTA                                    X350JPX5-TOTAL-SINIES     -
LTA                                    X350JPX5-GASTOS           -
LTA                                    X350JPX5-PBPAGADA-EMP) *
LTA                                    X350JXX2-EPNETA) /
LTA                                    X350JPX5-PRIMA-ANUAL
      *D*
      *       DISPLAY 'WS-IMPORTE-REPARTOS ' WS-IMPORTE-REPARTOS
      *F*

              END-IF
           ELSE

                  OPEN INPUT ENTRADA-TASE-LTP
                  PERFORM
                    UNTIL X350JXX2-KPOLIZA EQUAL
                          X350JPX5-KPOLIZA AND
                          X350JXX2-KSUBPOL EQUAL
                          X350JPX5-KSUBPOL AND
                          X350M004-KGRUPEMP-PS(3:3) EQUAL
                          X350JPX5-GEMPRESA

                  PERFORM 2703-LEER-TOTALES-LTP
                  END-PERFORM
                    CLOSE ENTRADA-TASE-LTP

                  OPEN INPUT ENTRADA-TASE-LTP
                  PERFORM 2703-LEER-TOTALES-LTP
                    UNTIL X350JXX2-KPOLIZA EQUAL
                          X350JPX5-KPOLIZA AND
                          X350JXX2-KSUBPOL EQUAL
                          X350JPX5-KSUBPOL AND
                          X350M004-KGRUPEMP-PS(3:3) EQUAL
                          X350JPX5-GEMPRESA
                    CLOSE ENTRADA-TASE-LTP

              IF X350JPX5-PRIMA-ANUAL <= ZEROES
                 MOVE ZEROES TO WS-IMPORTE-REPARTOS
              ELSE
      *D*
      *    DISPLAY 'X350JPX5-PRIMA-PERIOD ' X350JPX5-PRIMA-PERIOD
      *    DISPLAY 'X350JPX5-TOTAL-SINIES ' X350JPX5-TOTAL-SINIES
      *    DISPLAY 'X350JPX5-GASTOS       ' X350JPX5-GASTOS
      *    DISPLAY 'X350JPX5-PBPAGADA-EMP ' X350JPX5-PBPAGADA-EMP
      *    DISPLAY 'WS-IMPORTE-PRI        ' WS-IMPORTE-PRI
      *    DISPLAY 'X350JPX5-PRIMA-ANUAL  ' X350JPX5-PRIMA-ANUAL
      *F*
LTA              COMPUTE WS-IMPORTE-REPARTOS =
LTA                                   ((X350JPX5-PRIMA-PERIOD    -
LTA                                    X350JPX5-TOTAL-SINIES     -
LTA                                    X350JPX5-GASTOS           -
LTA                                    X350JPX5-PBPAGADA-EMP) *
LTA                                    X350JXX2-EPNETA) /
LTA                                    X350JPX5-PRIMA-ANUAL
      *D*
      *       DISPLAY 'WS-IMPORTE-REPARTOS ' WS-IMPORTE-REPARTOS
      *F*

              END-IF
           END-IF

210524*        MOVE X350JPX5-PBPAGADA-EMP TO RSAL-PBPAGA-TOT-EMPR

               MOVE WS-IMPORTE-REPARTOS    TO X350JPX6-INGRESOS-GASTOS

               IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
                  MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
               END-IF

               COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
                                            = WS-IMPORTE-REPARTOS


                COMPUTE WS-NETA-COASEGUR ROUNDED
                                         = (WS-IMPORTE-REPARTOS *
110624*                                     X350JPX3-PCOASE) / 100
110624                                      X350JXX2-PCOASE) / 100
      *D*
      *       DISPLAY 'WS-IMPORTE-REPARTOS ' WS-IMPORTE-REPARTOS
      *       DISPLAY 'X350JXX2-PCOASE     ' X350JPX3-PCOASE
      *F*
                MOVE WS-NETA-COASEGUR         TO X350JPX6-NETA-COASEGUR

                IF X350JPX6-NETA-COASEGUR NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-NETA-COASEGUR
                END-IF

                COMPUTE RSAL-NETA-COASEGUR ROUNDED
                                           = WS-NETA-COASEGUR
      *D*
      *       DISPLAY 'WS-NETA-COASEGUR    ' WS-NETA-COASEGUR
      *F*

                MOVE RSAL-P-PB-APLICAR  TO AUX-PB-TECNICA

                COMPUTE RSAL-PB-TECNICA ROUNDED
                                        = (WS-IMPORTE-REPARTOS *
210524*                                X350M004-PREAJPB-PS) / 100
210524                               WS-PRREAPB-PS-ACTUAL) / 100
                COMPUTE X350JPX6-PB-TECNICA = (WS-IMPORTE-REPARTOS *
210524*                                X350M004-PREAJPB-PS) / 100
210524                               WS-PRREAPB-PS-ACTUAL) / 100

                IF X350JPX6-PB-TECNICA NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-PB-TECNICA
                END-IF

110624*         MOVE X350JPX3-PCOASE          TO AUX-PCOASE
110624          MOVE X350JXX2-PCOASE          TO AUX-PCOASE
                COMPUTE X350JPX6-DEV-NETCOASE = (WS-NETA-COASEGUR *
                                       AUX-PB-TECNICA) / 100

                IF X350JPX6-DEV-NETCOASE < ZEROES
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                   MOVE ZEROES TO RSAL-DEV-NETCOASE
                ELSE
                   COMPUTE RSAL-DEV-NETCOASE ROUNDED
                                          = (WS-NETA-COASEGUR *
                                          AUX-PB-TECNICA) / 100
                END-IF

                IF X350JPX6-DEV-NETCOASE NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                END-IF


210524*        MOVE X350JPX5-PBPAGADA-EMP TO RSAL-PBPAGA-TOT-EMPR

               MOVE WS-IMPORTE-REPARTOS    TO X350JPX6-INGRESOS-GASTOS

               IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
                  MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
               END-IF

               COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
                                            = WS-IMPORTE-REPARTOS


                COMPUTE WS-NETA-COASEGUR ROUNDED
                                         = (WS-IMPORTE-REPARTOS *
110624*                                     X350JPX3-PCOASE) / 100
110624                                      X350JXX2-PCOASE) / 100
                MOVE WS-NETA-COASEGUR         TO X350JPX6-NETA-COASEGUR

                IF X350JPX6-NETA-COASEGUR NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-NETA-COASEGUR
                END-IF

                COMPUTE RSAL-NETA-COASEGUR ROUNDED
                                           = WS-NETA-COASEGUR

                MOVE RSAL-P-PB-APLICAR  TO AUX-PB-TECNICA

                COMPUTE RSAL-PB-TECNICA ROUNDED
                                        = (WS-IMPORTE-REPARTOS *
210524*                                X350M004-PREAJPB-PS) / 100
210524                               WS-PRREAPB-PS-ACTUAL) / 100
                COMPUTE X350JPX6-PB-TECNICA = (WS-IMPORTE-REPARTOS *
210524*                                X350M004-PREAJPB-PS) / 100
210524                               WS-PRREAPB-PS-ACTUAL) / 100

                IF X350JPX6-PB-TECNICA NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-PB-TECNICA
                END-IF

110624          MOVE X350JXX2-PCOASE          TO AUX-PCOASE
110624*         MOVE X350JPX3-PCOASE          TO AUX-PCOASE

                COMPUTE X350JPX6-DEV-NETCOASE = (WS-NETA-COASEGUR *
                                       AUX-PB-TECNICA) / 100

                IF X350JPX6-DEV-NETCOASE < ZEROES
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                   MOVE ZEROES TO RSAL-DEV-NETCOASE
                ELSE
                   COMPUTE RSAL-DEV-NETCOASE ROUNDED
                                          = (WS-NETA-COASEGUR *
                                          AUX-PB-TECNICA) / 100
                END-IF

                IF X350JPX6-DEV-NETCOASE NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                END-IF

           IF RSAL-PGLOBAL = '0'
RF001         MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA-REF1
              MOVE '/'                      TO RSAL-SEPARADOR
              MOVE '0000'                   TO RSAL-SUBPOL-REF1
           END-IF

           IF RSAL-POLIZA-REF1 = '0000000'
              MOVE SPACES TO RSAL-POLIZA-REF1
              MOVE SPACES TO RSAL-SEPARADOR
              MOVE SPACES TO RSAL-SUBPOL-REF1
           END-IF

           PERFORM 2200-RECUPERAR-UOA
110624*    PERFORM 2300-OBTENER-NEW-GAP
110624     PERFORM 2300-OBTENER-NEW-GAP-MOV

           IF RSAL-REAJUSTE = 'S' AND  RSAL-REAJUSTE-PB = SPACES
              MOVE 'POL CON REAJUSTE CON SU TIPO PENDIENTE'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'B350JA12'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE '01'                TO X350J000-ID-ERROR-MOD
              PERFORM 2400-ESCRIBIR-ERRORES
           ELSE
              IF RSAL-REAJUSTE = 'S' AND
                 RSAL-REAJUSTE-PB NOT = SPACES
                 MOVE 'POL CON REAJUSTE CON SU TIPO INCONGRUENTE'
                                           TO X350J000-DESC-ERROR
                  MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                  MOVE 'B350JA12'          TO X350J000-ID-MODULO
                  MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                  MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                  MOVE 'W'                 TO X350J000-NIVEL
                  MOVE '02'                TO X350J000-ID-ERROR-MOD
                  PERFORM 2400-ESCRIBIR-ERRORES
                END-IF
           END-IF

           IF RSAL-REAJUSTE = 'N'           AND
              RSAL-REAJUSTE-PB NOT = SPACES OR
              AUX-PCALCULOPB-S NOT = ZEROES OR
              AUX-PPORCEN-REAL NOT = ZEROES
              MOVE 'POL SIN REAJUSTE PERO CON DATOS DEL MISMO'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'B350JA12'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE '03'                TO X350J000-ID-ERROR-MOD

              PERFORM 2400-ESCRIBIR-ERRORES
           END-IF

      *    PERFORM 5214-CALCULOS-LTP
PBTEC *    MOVE WS-IMPORTE-PBP         TO RSAL-PBPAGA

      *D*
      *          DISPLAY 'RSAL-FECHA CIERRE: ' WS-FECSIN-CIERRE
      *          DISPLAY 'RSAL-FECHA HASTA: ' WS-FECSIN-HASTA
      *F*
LUNA       MOVE RSAL-FCIERREA TO WS-FECSIN-ACIERRE
LUNAJ      MOVE RSAL-FCIERREM TO WS-FECSIN-MCIERRE
LUNAJ      MOVE RSAL-FCIERRED TO WS-FECSIN-DCIERRE
LUNAJ
LUNAJ      MOVE WS-FECSIN-DESDE TO RSAL-FECSIN-DESDE
LUNAJ      IF WS-FECSIN-CIERRE < WS-FECSIN-HASTA
LUNAJ         MOVE WS-FECSIN-CIERRE TO RSAL-FECSIN-HASTA
LUNAJ      ELSE
LUNAJ         MOVE WS-FECSIN-HASTA TO RSAL-FECSIN-HASTA
LUNAJ      END-IF

JOSENE     IF WS-FECSIN-CIERRE = RSAL-FECSIN-HASTA
JOSENE        MOVE RSAL-FECSIN-HASTA TO WS-FECSIN-HASTA-PLU-BON
JOSENE     END-IF

JOSENE     MOVE WS-FECSIN-DESDE TO WS-FECSIN-DESDE-PLU-BON

           WRITE REG-SALIDA1 FROM RSALIDA

      *    MOVE WS-POLIZA-2 TO WS-POLIZA-2-ANT
      *    MOVE WS-SUBPOL-2 TO WS-SUBPOL-2-ANT
      *    MOVE WS-POLIZA-4 TO WS-POLIZA-4-ANT
      *    MOVE WS-SUBPOL-4 TO WS-SUBPOL-4-ANT

           INITIALIZE RSALIDA

           MOVE X350JXX2                      TO X350JPX3
LTA        .
LTA   ******************************************************************10650099
       5000-GRABAR-SALIDA-LTB.                                          10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5000-GRABAR-SALIDA-LTB'
      *F*
           MOVE '3000'                   TO RSAL-MODELO

           MOVE X350JPX3-FRENO-DESDE TO WS-FECHA-DESDE-ALFA-LTA
           MOVE X350JPX3-FRENO-HASTA TO WS-FECHA-HASTA-ALFA-LTA

           IF X350JPX3-FRENO-HASTA <= R350T000-FFINAL
              MOVE X350JPX3-FRENO-HASTA  TO WS-FEC-AUX-LTP-D
              MOVE X350JPX3-FRENO-AHASTA TO AUX-ANNO-LTP
              ADD  1                     TO AUX-ANNO-LTP
              MOVE AUX-ANNO-LTP          TO X350JPX3-FRENO-AHASTA
              MOVE X350JPX3-FRENO-HASTA  TO WS-FEC-AUX-LTP-H
           END-IF

JOSE       PERFORM CALCULAR-ANNOS-TRANS-MVT-LTB

           PERFORM 3301-RECUPERAR-ALFA-MVT-LTB

           MOVE X350JPX3-CCANAL          TO RSAL-CANAL
                                            X350JPX6-CANAL
           MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA
           MOVE X350JPX3-KSUBPOL         TO RSAL-SUBPOLIZA
           MOVE X350JPX3-KSUSCRI         TO RSAL-SUSCRIP

           MOVE X350JPX3-FEFECTO-AINIPOL TO RSAL-FORIGENASUSCRI
           MOVE X350JPX3-FEFECTO-MINIPOL TO RSAL-FORIGENMSUSCRI
           MOVE X350JPX3-FEFECTO-DINIPOL TO RSAL-FORIGENDSUSCRI

           MOVE X350JPX3-KRAMO           TO RSAL-RAMO
                                            X350JPX6-RAMO
           MOVE X350JPX3-MODALI          TO RSAL-MODALIDAD
                                            X350JPX6-MODALIDAD
           MOVE X350JPX3-VIGOR-ANUL      TO RSAL-CESTADOASEG
           MOVE '00'                     TO RSAL-PREGRUPO(1:2)
           MOVE X350JPX3-CARTINV         TO RSAL-PREGRUPO(3:2)
           MOVE X350JPX3-CASADA          TO RSAL-CASADO
           MOVE X350JPX3-PINTER-TECBASE  TO RSAL-I1BTI

      *    MOVE WS-ANNO-DESDE-TRAMO      TO RSAL-FRENO-ADESDE
      *    MOVE WS-MES-DESDE-TRAMO       TO RSAL-FRENO-MDESDE
      *    MOVE WS-DIA-DESDE-TRAMO       TO RSAL-FRENO-DDESDE
PLURIN       MOVE WS-FRENO-DESDE-ABONO-ACTUAL TO RSAL-FRENO-ADESDE
PLURIN       MOVE WS-FRENO-DESDE-MBONO-ACTUAL TO RSAL-FRENO-MDESDE
PLURIN       MOVE WS-FRENO-DESDE-DBONO-ACTUAL TO RSAL-FRENO-DDESDE

           MOVE WS-ANNO-HASTA-TRAMO   TO RSAL-FRENO-AHASTA
           MOVE WS-MES-HASTA-TRAMO    TO RSAL-FRENO-MHASTA
           MOVE WS-DIA-HASTA-TRAMO    TO RSAL-FRENO-DHASTA
           MOVE WS-RENOVADA           TO RSAL-RENOVADA
                                         X350JPX6-RENOVADA

      *    IF WS-FECHA-LTA-H         >  R350T000-FFINAL
      *    OR WS-FECHA-LTA-H         =  R350T000-FFINAL
      *       MOVE 'S'                   TO RSAL-RENOVADA
      *                                     X350JPX6-RENOVADA
      *       MOVE 'S' TO WS-RENOVADA
      *    ELSE
      *       MOVE 'N'                   TO RSAL-RENOVADA
      *                                     X350JPX6-RENOVADA
      *       MOVE 'N' TO WS-RENOVADA
      *    END-IF

JOSEN           IF WS-FECHA-HASTA-TRAMO < R350T000-FFINAL
JOSEN              MOVE WS-ANNO-HASTA-TRAMO TO RSAL-FCIERREA
                                                 X350JPX6-FCIERREA
                   MOVE WS-MES-HASTA-TRAMO  TO RSAL-FCIERREM
                                                 X350JPX6-FCIERREM
JOSEN              MOVE WS-DIA-HASTA-TRAMO  TO RSAL-FCIERRED
                                                 X350JPX6-FCIERRED
JOSEN           ELSE
                   MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
                                                 X350JPX6-FCIERREA
                   MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
                                                 X350JPX6-FCIERREM
                   MOVE R350T000-FDFINAL         TO RSAL-FCIERRED
                                                 X350JPX6-FCIERRED
JOSEN           END-IF
      *    MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
      *                                     X350JPX6-FCIERREA
      *    MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
      *                                     X350JPX6-FCIERREM
      *    MOVE R350T000-FDFINAL         TO RSAL-FCIERRED
      *                                     X350JPX6-FCIERRED
           MOVE 'BONO'                   TO RSAL-LTA
           MOVE X350JPX3-FCOBRO          TO RSAL-FORMAPAGO
           MOVE X350JPX3-TIPO-COASE      TO RSAL-COASEGURO
           MOVE X350JPX3-PCOASE          TO RSAL-PCOASEGURO
           MOVE X350JPX3-PAGADOR         TO RSAL-PGLOBAL

021125*    IF X350JXX3-CERTI EQUAL '000000' AND                         000
021125*       X350JXX3-ASEG-MECANIZADOS = 'N'                           000
021125     IF X350JPX3-CERTI EQUAL '000000' AND                         000
021125        X350JPX3-ASEG-MECANIZADOS = 'N'                           000
              MOVE 'S' TO RSAL-IMNOMINADA                               000
           ELSE
              MOVE 'N' TO RSAL-IMNOMINADA                               000
           END-IF

           MOVE X350JPX3-NASEGURADO      TO RSAL-NASEGURADOS

           IF X350JPX3-VIGOR-ANUL  = 'A'
              MOVE X350JPX3-ANO-ANUL     TO RSAL-FECAANULACION
              MOVE X350JPX3-MES-ANUL     TO RSAL-FECMANULACION
              MOVE X350JPX3-DIA-ANUL     TO RSAL-FECDANULACION
              MOVE X350JPX3-MOTIVO-ANU   TO RSAL-MOTIVO
           END-IF

           MOVE WS-ALFA                  TO RSAL-ALFA

130624     MOVE WS-KGRUPEMP-BS-ACTUAL TO X350M004-KGRUPEMP-BS
EMPMAR     IF X350M004-KGRUPEMP-BS = ZEROS
EMPMAR        MOVE SPACES                TO X350M004-KGRUPEMP-BS
EMPMAR     END-IF

           MOVE X350M004-KGRUPEMP-BS(3:3) TO RSAL-GEMPRESA

      *    IF AUX-TIP-REAJUSTE = '1' OR 'T'
      *       MOVE 'S'                      TO RSAL-REAJUSTE
      *    ELSE
      *       MOVE 'N'                      TO RSAL-REAJUSTE
      *    END-IF

           MOVE AUX-CCOBON-REAJUS        TO RSAL-REAJUSTE

           MOVE AUX-REAJUSTE-PB          TO RSAL-REAJUSTE-PB

           MOVE WS-REAJREC               TO RSAL-REAJUSTE-REC
           MOVE AUX-TIP-REAJUSTE         TO RSAL-TIP-REAJUSTE
           MOVE AUX-NANIOPER             TO RSAL-APERDIDAS
           MOVE AUX-NANIOBEN             TO RSAL-ABENEFICI

           IF X350X098-IANTICIPO-S > ZEROES
              MOVE 'S'                   TO RSAL-ANTICIPO-PB
           ELSE
              MOVE 'N'                   TO RSAL-ANTICIPO-PB
           END-IF

           MOVE AUX-PANTICIPO-S          TO RSAL-P-ANTICIPO
           MOVE AUX-CCUADROBAREMO-S      TO RSAL-BAREMO
           MOVE AUX-PPORCEN-REAL         TO RSAL-REAJUSTEO
210524*    MOVE X350M004-PPBBONO-BS      TO RSAL-P-PB-APLICAR
210524*    MOVE X350M004-PPBBONO-BS      TO RSAL-P-REAUSTE-PB
210524*    MOVE X350M004-PPBBONO-BS      TO AUX-PCALCULOPB-S
210524*    MOVE X350M004-PGASBONO-BS     TO RSAL-P-GASTOS-PB
210524     MOVE WS-PRREAPB-BS-ACTUAL     TO RSAL-P-PB-APLICAR
210524     MOVE WS-PRREAPB-BS-ACTUAL     TO RSAL-P-REAUSTE-PB
210524     MOVE WS-PRREAPB-BS-ACTUAL     TO AUX-PCALCULOPB-S
210524     MOVE WS-PGASTOS-BS-ACTUAL     TO RSAL-P-GASTOS-PB

      *D*
      *    DISPLAY 'WS-IMPORTE-205 ' WS-IMPORTE-205
      *    DISPLAY 'WS-IMPORTE-PRI ' WS-IMPORTE-PRI
      *F*
           COMPUTE WS-IMPORTE-LTB = WS-IMPORTE-205 + WS-IMPORTE-PRI

      *D*
      *    DISPLAY 'WS-IMPORTE-LTB ' WS-IMPORTE-LTB
      *F*

      *    COMPUTE RSAL-PRIMA-RECIBOS ROUNDED = WS-IMPORTE-LTB

           COMPUTE WS-IMPORTE-205 ROUNDED = WS-IMPORTE-205 +
JOSEN                                       WS-IMPORTE-205-BONO
130824     COMPUTE WS-IMPORTE-902 ROUNDED = WS-IMPORTE-902 +
130824                                 WS-IMPORTE-902-BONO

           MOVE WS-IMPORTE-205           TO X350JPX6-PRIMA-RECIBOS

           COMPUTE RSAL-APUNTE-RECIBO ROUNDED = WS-IMPORTE-902
           COMPUTE RSAL-PRIMA-RECIBOS ROUNDED = WS-IMPORTE-205


           MOVE WS-IMPORTE-902           TO X350JPX6-APUNTE-RECIBO

      *D*
      *    DISPLAY 'WS-IMP-PERI-CAL3 ' WS-IMP-PERI-CAL3
      *    DISPLAY 'WS-ALFA        ' WS-ALFA
      *    DISPLAY 'WS-IMPORTE-205 ' WS-IMPORTE-205
      *    DISPLAY 'WS-IMPORTE-205-BONO ' WS-IMPORTE-205-BONO
      *    DISPLAY 'WS-IMPORTE-902 ' WS-IMPORTE-902
      *    DISPLAY 'WS-IMPORTE-902-BONO ' WS-IMPORTE-902-BONO
      *    DISPLAY 'WS-IMPORTE-902-BONO ' WS-IMPORTE-902-BONO
      *F*

110624       IF WS-RENOVADA = 'S'
110624          MOVE WS-ALFA TO RSAL-ALFA
110624       ELSE
240524          IF WS-IMP-PERI-CAL3 EQUAL ZEROES
240524             MOVE 1 TO WS-ALFA
240524                       RSAL-ALFA
                ELSE
                   MOVE WS-ALFA TO RSAL-ALFA
240524          END-IF
110624       END-IF
PBTEC      COMPUTE WS-IMP-PERI-CAL3   ROUNDED
PBTEC                   = (WS-IMP-PERI-CAL3) * WS-ALFA

           COMPUTE WS-IMP-PERI-CALC = WS-IMP-PERI-CAL1 +
                   WS-IMP-PERI-CAL2 + WS-IMP-PERI-CAL3

JOSENE        IF WS-ALFA NOT EQUAL 1
JOSENE           COMPUTE WS-IMPORTE-205-BONO ROUNDED
                         = (WS-IMPORTE-205-BONO) * WS-ALFA
JOSENE        END-IF
JOSENE        IF WS-ALFA NOT EQUAL 1
JOSENE           COMPUTE WS-IMPORTE-902-BONO ROUNDED
                         = (WS-IMPORTE-902-BONO) * WS-ALFA
JOSENE        END-IF

LTA        COMPUTE WS-IMPORTE-PERIOD-LTA ROUNDED
JOSENE             = WS-IMP-PERI-CALC + WS-IMPORTE-205-BONO
130824                                + WS-IMPORTE-902-BONO

100124*    IF SW-SI-LTA-FINAL
100124*       COMPUTE WS-IMPORTE-PERIOD-LTA ROUNDED = X350JPX3-EPNETA
100124*    END-IF
      *D*
      *         DISPLAY ' WS-IMPORTE-PERIOD-LTA '  WS-IMPORTE-PERIOD-LTA
      *F*

           COMPUTE WS-IMPORTE-PERIOD ROUNDED
JOSENE             = WS-IMP-PERI-CALC + WS-IMPORTE-205-BONO
130824                                + WS-IMPORTE-902-BONO
      *D*
      *         DISPLAY ' WS-ALFA               '  WS-ALFA
      *         DISPLAY ' WS-IMPORTE-PERIOD-LTA '  WS-IMPORTE-PERIOD-LTA
      *         DISPLAY ' WS-IMPORTE-PERIOD     '  WS-IMPORTE-PERIOD
      *F*

           COMPUTE RSAL-PRIMA-PERIOD ROUNDED = WS-IMPORTE-PERIOD-LTA

           MOVE WS-IMPORTE-PERIOD        TO X350JPX6-PRIMA-PERIOD

           COMPUTE WS-IMP-GASTOS = (WS-IMPORTE-PERIOD *
210524*                     X350M004-PGASBONO-BS) / 100
210524                    WS-PGASTOS-BS-ACTUAL) / 100

LTA        COMPUTE WS-IMP-GASTOS-LTA = (WS-IMPORTE-PERIOD-LTA *
210524*                     X350M004-PGASBONO-BS) / 100
210524                    WS-PGASTOS-BS-ACTUAL) / 100

      *D*
      *    DISPLAY 'WS-IMP-GASTOS  ' WS-IMP-GASTOS
      *    DISPLAY 'WS-IMP-GASTOS-LTA ' WS-IMP-GASTOS-LTA
      *F*
           MOVE WS-IMP-GASTOS            TO X350JPX6-GASTOS

           COMPUTE RSAL-GASTOS ROUNDED = WS-IMP-GASTOS-LTA

           IF SW-SI-LTBA
              CONTINUE
           ELSE
              MOVE WS-IMPORTE-206-SINI TO WS-IMPORTE-206
           END-IF
MF001 *    MOVE WS-IMPORTE-206           TO X350JPX6-TOTAL-SINIES
           MOVE TB-ACUMULADO-206 (5)     TO X350JPX6-TOTAL-SINIES

MF001 *    COMPUTE RSAL-TOTAL-SINIES ROUNDED = WS-IMPORTE-206
           COMPUTE RSAL-TOTAL-SINIES ROUNDED = TB-ACUMULADO-206 (5)

           MOVE X350X098-IARRASTRE-S     TO RSAL-IMP-ARRAS-PERD

      *    MOVE X350M004-PGASBONO-BS     TO RSAL-P-GASTOS-PB

jose16*    COMPUTE RSAL-PRIMA-ANUAL ROUNDED =  WS-IMPORTE-PRI
josen      COMPUTE RSAL-PRIMA-ANUAL ROUNDED = X350JPX3-EPNETA

JOSE16     MOVE X350JPX3-EPNETA          TO X350JPX6-PRIMA-ANUAL
           MOVE X350X098-IARRASTRE-S TO RSAL-IMP-ARRAS-PERD

           MOVE X350JPX3-EPNETA          TO X350JPX6-PRIMA-ANUAL
           MOVE X350X098-IARRASTRE-S TO RSAL-IMP-ARRAS-PERD
           MOVE ZEROES                   TO RSAL-NETA-COASEGUR
                                            X350JPX6-NETA-COASEGUR
           MOVE SPACES TO X350JPX5-GEMPRESA

EMPMAR**   IF X350JPX3-KGRUEMP = '000'
EMPMAR     IF X350M004-KGRUPEMP-BS = SPACES
              MOVE SPACES           TO X350JPX3-KGRUEMP
           END-IF

EMPMAR**   IF X350JPX3-KGRUEMP NOT = SPACES
EMPMAR     IF X350M004-KGRUPEMP-BS NOT = SPACES
             MOVE X350JPX4-NASEGURADO    TO RSAL-ASE-TOT-EMPRESA
           END-IF

           MOVE SPACES TO X350JPX5-GEMPRESA

EMPMAR**   IF X350JPX3-KGRUEMP NOT = SPACES
EMPMAR     IF X350M004-KGRUPEMP-BS NOT = SPACES
              OPEN INPUT ENTRADA-TASE-LTB
              PERFORM 2703-LEER-TOTALES-LTB
EMPMAR**        UNTIL X350JPX3-KGRUEMP EQUAL
EMPMAR          UNTIL X350M004-KGRUPEMP-BS(3:3) EQUAL
                      X350JPX5-GEMPRESA
                CLOSE ENTRADA-TASE-LTB

      *D*
      *        DISPLAY 'X350JPX3-KGRUEMP         ' X350JPX3-KGRUEMP
      *        DISPLAY 'X350JPX5-PRIMA-PERIOD ' X350JPX5-PRIMA-PERIOD
      *F*
              MOVE X350JPX5-PBPAGADA-EMP    TO RSAL-PBPAGA-TOT-EMPR
              MOVE X350JPX5-PRIMA-PERIOD    TO RSAL-PRI-TOT-EMPRESA

              IF X350JPX5-TOTAL-SINIES = ZEROES
                 MOVE AUX-SINIESTROS-LTP    TO RSAL-SIN-TOT-EMPRESA
              ELSE
                 MOVE X350JPX5-TOTAL-SINIES TO RSAL-SIN-TOT-EMPRESA
              END-IF

              COMPUTE RSAL-GASTOS-TOT-EMPR = (X350JPX5-PRIMA-PERIOD *
210524*                        X350M004-PGASBONO-BS) / 100
210524                       WS-PGASTOS-BS-ACTUAL) / 100

      *D*
      *       DISPLAY 'bono  '
      *       DISPLAY 'X350JPX5-PRIMA-PERIOD ' X350JPX5-PRIMA-PERIOD
      *       DISPLAY 'WS-IMPORTE-206        ' WS-IMPORTE-206
      *       DISPLAY 'X350JPX5-TOTAL-SINIES ' X350JPX5-TOTAL-SINIES
      *       DISPLAY 'X350JPX5-GASTOS       ' X350JPX5-GASTOS
      *       DISPLAY 'X350JPX5-PBPAGADA-EMP ' X350JPX5-PBPAGADA-EMP
      *       DISPLAY 'WS-IMPORTE-PRI        ' WS-IMPORTE-PRI
      *       DISPLAY 'X350JPX5-PRIMA-ANUAL  ' X350JPX5-PRIMA-ANUAL
      *F*

              COMPUTE WS-IMPORTE-REPARTOS = ((X350JPX5-PRIMA-PERIOD -
                                              X350JPX5-TOTAL-SINIES -
                                              X350JPX5-GASTOS   -
                                              X350JPX5-PBPAGADA-EMP) *
                                              X350JPX3-EPNETA) /
                                              X350JPX5-PRIMA-ANUAL

      *D*
      *       DISPLAY 'WS-IMPORTE-REPARTOS ' WS-IMPORTE-REPARTOS
      *F*
              MOVE WS-IMPORTE-REPARTOS TO X350JPX6-INGRESOS-GASTOS

              IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
                 MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
              END-IF

               COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
                                         = WS-IMPORTE-REPARTOS

               COMPUTE WS-NETA-COASEGUR ROUNDED
                                         = (WS-IMPORTE-REPARTOS *
                                            X350JPX3-PCOASE) / 100
                MOVE WS-NETA-COASEGUR         TO X350JPX6-NETA-COASEGUR

                IF X350JPX6-NETA-COASEGUR NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-NETA-COASEGUR
                END-IF

                COMPUTE RSAL-NETA-COASEGUR ROUNDED
                                           = WS-NETA-COASEGUR

                MOVE RSAL-P-PB-APLICAR  TO AUX-PB-TECNICA

                COMPUTE RSAL-PB-TECNICA ROUNDED
                                        = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100
                COMPUTE X350JPX6-PB-TECNICA = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100

                IF X350JPX6-PB-TECNICA NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-PB-TECNICA
                END-IF

                MOVE X350JPX3-PCOASE          TO AUX-PCOASE

                COMPUTE X350JPX6-DEV-NETCOASE = (WS-NETA-COASEGUR *
210524*                                X350M004-PPBBONO-BS) / 100
210524                               WS-PRREAPB-BS-ACTUAL) / 100

      *D*
      *         DISPLAY 'WS-NETA-COASEGUR ' WS-NETA-COASEGUR
      *         DISPLAY 'X350M004-PPBBONO-BS ' X350M004-PPBBONO-BS
      *F*
                IF X350JPX6-DEV-NETCOASE < ZEROES
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                   MOVE ZEROES TO RSAL-DEV-NETCOASE
                ELSE
                   COMPUTE RSAL-DEV-NETCOASE ROUNDED
                                          = (WS-NETA-COASEGUR *
210524*                                  X350M004-PPBBONO-BS) / 100
210524                                 WS-PRREAPB-BS-ACTUAL) / 100
      *D*
      *         DISPLAY 'RSAL-DEV-NETCOASE ' RSAL-DEV-NETCOASE
      *F*
                END-IF

                IF X350JPX6-DEV-NETCOASE NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                END-IF


           ELSE
      *D*
      *        DISPLAY 'ME VOY POR ELSE SI LTB'
      *        DISPLAY 'X350JPX4-KPOLIZA       ' X350JPX4-KPOLIZA
      *        DISPLAY 'X350JPX3-KPOLIZA       ' X350JPX3-KPOLIZA
      *        DISPLAY 'X350JPX3-KSUBPOL       ' X350JPX3-KSUBPOL
      *        DISPLAY 'X350JPX4-KSUBPOL       ' X350JPX4-KSUBPOL
      *        DISPLAY 'X350JPX3-KGRUEMP       ' X350JPX3-KGRUEMP
      *        DISPLAY 'X350JPX4-KGRUEMP       ' X350JPX4-KGRUEMP
      *F*
               OPEN INPUT ENTRADA-TASE-NE
                  PERFORM 2703-LEER-TOTALES-ASEGURADO-NE
                    UNTIL X350JPX3-KPOLIZA EQUAL
EMPMAR*                   X350JPX5-KPOLIZA AND
EMPMAR                    X350JPX4-KPOLIZA AND
                          X350JPX3-KSUBPOL EQUAL
                          X350JPX4-KSUBPOL AND
EMPMAR*                   X350JPX3-KGRUEMP EQUAL
EMPMAR                    X350M004-KGRUPEMP-BS(3:3) EQUAL
                          X350JPX4-KGRUEMP
                    CLOSE ENTRADA-TASE-NE

               OPEN INPUT ENTRADA-TASE-CP
                  PERFORM 2703-LEER-TOTALES-ASEGURADO-CP
                    UNTIL X350JPX3-KPOLIZA EQUAL
                          X350JPX5-KPOLIZA AND
                          X350JPX3-KSUBPOL EQUAL
                          X350JPX5-KSUBPOL
                    CLOSE ENTRADA-TASE-CP

      *D*
      *        DISPLAY 'X350JPX5-PRIMA-ANUAL '  X350JPX5-PRIMA-ANUAL
      *F*
               IF X350JPX5-PRIMA-ANUAL <= ZEROES
                  MOVE ZEROES TO WS-IMPORTE-REPARTOS
               ELSE
                  COMPUTE WS-IMPORTE-REPARTOS = ((WS-IMPORTE-PERIOD -
MF001 *                                         WS-IMPORTE-206    -
MF001                                           TB-ACUMULADO-206 (5) -
                                                WS-IMP-GASTOS     -
011225                                          WS-IMPORTE-PBP) *
                                                X350JPX5-PRIMA-ANUAL) /
                                                X350JPX5-PRIMA-ANUAL

               END-IF

      *D*
      *        DISPLAY 'WS-IMPORTE-PERIOD    '  WS-IMPORTE-PERIOD
      *        DISPLAY 'WS-IMPORTE-206       '  WS-IMPORTE-206
      *        DISPLAY 'WS-IMP-GASTOS        '  WS-IMP-GASTOS
      *        DISPLAY 'X350JPX5-PRIMA-ANUAL '  X350JPX5-PRIMA-ANUAL
      *        DISPLAY 'WS-IMPORTE-REPARTOS  '  WS-IMPORTE-REPARTOS
      *F*
                   MOVE WS-IMPORTE-REPARTOS TO X350JPX6-INGRESOS-GASTOS

                   IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
                      MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
                   END-IF

                   COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
                                                = WS-IMPORTE-REPARTOS


                COMPUTE WS-NETA-COASEGUR ROUNDED
                                         = (WS-IMPORTE-REPARTOS *
                                            X350JPX3-PCOASE) / 100
                MOVE WS-NETA-COASEGUR         TO X350JPX6-NETA-COASEGUR

                IF X350JPX6-NETA-COASEGUR NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-NETA-COASEGUR
                END-IF

                COMPUTE RSAL-NETA-COASEGUR ROUNDED
                                           = WS-NETA-COASEGUR

                MOVE RSAL-P-PB-APLICAR  TO AUX-PB-TECNICA
      *D*
      *    DISPLAY 'RSAL-P-PB-APLICAR ' RSAL-P-PB-APLICAR
      *    DISPLAY 'WS-NETA-COASEGUR  ' WS-NETA-COASEGUR
      *    DISPLAY 'WS-IMPORTE-REPARTOS ' WS-IMPORTE-REPARTOS
      *F*


081225          COMPUTE RSAL-PB-TECNICA ROUNDED
081225                                  = (WS-IMPORTE-REPARTOS *
081225                                     AUX-PB-TECNICA) / 100
                COMPUTE X350JPX6-PB-TECNICA = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100

                IF X350JPX6-PB-TECNICA NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-PB-TECNICA
                END-IF

      *D*
      *    DISPLAY 'RSAL-PB-TECNICA     ' RSAL-PB-TECNICA
      *F*
                MOVE X350JPX3-PCOASE          TO AUX-PCOASE

                COMPUTE X350JPX6-DEV-NETCOASE = (WS-NETA-COASEGUR *
                                       AUX-PB-TECNICA) / 100

                IF X350JPX6-DEV-NETCOASE < ZEROES
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                   MOVE ZEROES TO RSAL-DEV-NETCOASE
                ELSE
                   COMPUTE RSAL-DEV-NETCOASE ROUNDED
                                          = (WS-NETA-COASEGUR *
                                          AUX-PB-TECNICA) / 100
                END-IF

                IF X350JPX6-DEV-NETCOASE NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                END-IF

      *
      *
050724*       COMPUTE WS-IMPORTE-REPARTOS = ((X350JPX5-PRIMA-PERIOD -
050724*                                       WS-IMPORTE-206    -
200524*                                       AUX-GASTOS-LTA) *
050724*                                       X350JPX5-GASTOS) *
050724*                                       X350JPX5-PRIMA-ANUAL) /
050724*                                       X350JPX5-PRIMA-ANUAL
050724*
050724*       MOVE WS-IMPORTE-REPARTOS TO X350JPX6-INGRESOS-GASTOS
050724*
050724*       IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
050724*          MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
050724*       END-IF
050724*
050724*           COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
050724*                                        = WS-IMPORTE-REPARTOS
050724*
050724*
050724*         COMPUTE WS-NETA-COASEGUR ROUNDED
050724*                                  = (WS-IMPORTE-REPARTOS *
050724*                                     X350JPX3-PCOASE) / 100
050724*         MOVE WS-NETA-COASEGUR         TO X350JPX6-NETA-COASEGUR
050724*
050724*         IF X350JPX6-NETA-COASEGUR NOT NUMERIC
050724*            MOVE ZEROES TO X350JPX6-NETA-COASEGUR
050724*         END-IF
050724*
050724*         COMPUTE RSAL-NETA-COASEGUR ROUNDED
050724*                                    = WS-NETA-COASEGUR
050724*
050724*         MOVE RSAL-P-PB-APLICAR  TO AUX-PB-TECNICA
050724*
050724*         COMPUTE RSAL-PB-TECNICA ROUNDED
050724*                                 = (WS-IMPORTE-REPARTOS *
050724*                                    AUX-PB-TECNICA) / 100
050724*         COMPUTE X350JPX6-PB-TECNICA = (WS-IMPORTE-REPARTOS *
050724*                                    AUX-PB-TECNICA) / 100
050724*D*
050724*    DISPLAY '5000-GRABAR-SALIDA-LTB-PB TECNICA'
050724*    DISPLAY 'WS-IMPORTE-REPARTOS     ' WS-IMPORTE-REPARTOS
050724*    DISPLAY 'AUX-PB-TECNICA          ' AUX-PB-TECNICA
050724*    DISPLAY 'RSAL-PB-TECNICA         ' RSAL-PB-TECNICA
050724*F*
050724*
050724*         IF X350JPX6-PB-TECNICA NOT NUMERIC
050724*            MOVE ZEROES TO X350JPX6-PB-TECNICA
050724*         END-IF
050724*
050724*         MOVE X350JPX3-PCOASE          TO AUX-PCOASE
050724*
050724*         COMPUTE X350JPX6-DEV-NETCOASE = (WS-NETA-COASEGUR *
050724*                                AUX-PB-TECNICA) / 100
050724*
050724*         IF X350JPX6-DEV-NETCOASE < ZEROES
050724*            MOVE ZEROES TO X350JPX6-DEV-NETCOASE
050724*            MOVE ZEROES TO RSAL-DEV-NETCOASE
050724*         ELSE
050724*            COMPUTE RSAL-DEV-NETCOASE ROUNDED
050724*                                   = (WS-NETA-COASEGUR *
050724*                                   AUX-PB-TECNICA) / 100
050724*         END-IF
050724*
050724*         IF X350JPX6-DEV-NETCOASE NOT NUMERIC
050724*            MOVE ZEROES TO X350JPX6-DEV-NETCOASE
050724*         END-IF
050724*
PBTEC      END-IF

           IF RSAL-PGLOBAL = '0'
              MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA-REF1
              MOVE '/'                      TO RSAL-SEPARADOR
              MOVE '0000'                   TO RSAL-SUBPOL-REF1
           END-IF

           IF RSAL-POLIZA-REF1 = '0000000'
              MOVE SPACES TO RSAL-POLIZA-REF1
              MOVE SPACES TO RSAL-SEPARADOR
              MOVE SPACES TO RSAL-SUBPOL-REF1
           END-IF



           PERFORM 2210-RECUPERAR-UOA-NR
           PERFORM 2300-OBTENER-NEW-GAP

           IF RSAL-REAJUSTE = 'S' AND  RSAL-REAJUSTE-PB = SPACES
              MOVE 'POL CON REAJUSTE CON SU TIPO PENDIENTE'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'B350JA12'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE '01'                TO X350J000-ID-ERROR-MOD
              PERFORM 2400-ESCRIBIR-ERRORES
           ELSE
              IF RSAL-REAJUSTE = 'S' AND
                 RSAL-REAJUSTE-PB NOT = SPACES
                 MOVE 'POL CON REAJUSTE CON SU TIPO INCONGRUENTE'
                                           TO X350J000-DESC-ERROR
                  MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                  MOVE 'B350JA12'          TO X350J000-ID-MODULO
                  MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                  MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                  MOVE 'W'                 TO X350J000-NIVEL
                  MOVE '02'                TO X350J000-ID-ERROR-MOD
                  PERFORM 2400-ESCRIBIR-ERRORES
                END-IF
           END-IF

           IF RSAL-REAJUSTE = 'N'           AND
              RSAL-REAJUSTE-PB NOT = SPACES OR
              AUX-PCALCULOPB-S NOT = ZEROES OR
              AUX-PPORCEN-REAL NOT = ZEROES
              MOVE 'POL SIN REAJUSTE PERO CON DATOS DEL MISMO'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'B350JA12'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE '03'                TO X350J000-ID-ERROR-MOD

              PERFORM 2400-ESCRIBIR-ERRORES
           END-IF

      *    PERFORM 5214-CALCULOS-LTB
           MOVE WS-IMPORTE-PBP         TO RSAL-PBPAGA

      *D*
      *          DISPLAY 'RSAL-FECHA CIERRE: ' WS-FECSIN-CIERRE
      *          DISPLAY 'RSAL-FECHA HASTA: ' WS-FECSIN-HASTA
      *F*
LUNA       MOVE RSAL-FCIERREA TO WS-FECSIN-ACIERRE
LUNA       MOVE RSAL-FCIERREM TO WS-FECSIN-MCIERRE
LUNA       MOVE RSAL-FCIERRED TO WS-FECSIN-DCIERRE
LUNA
1412       MOVE WS-FRENO-DESDE-BONO-ACTUAL TO WS-FECSIN-DESDE
LUNA       MOVE WS-FECSIN-DESDE TO RSAL-FECSIN-DESDE
LUNA       IF WS-FECSIN-CIERRE < WS-FECSIN-HASTA
LUNA          MOVE WS-FECSIN-CIERRE TO RSAL-FECSIN-HASTA
LUNA       ELSE
LUNA          MOVE WS-FECSIN-HASTA TO RSAL-FECSIN-HASTA
LUNA       END-IF

           WRITE REG-SALIDA1 FROM RSALIDA

           MOVE ZEROES TO WS-IMPORTE-PERIOD-LTA

      *    MOVE WS-POLIZA-2 TO WS-POLIZA-2-ANT
      *    MOVE WS-SUBPOL-2 TO WS-SUBPOL-2-ANT
      *    MOVE WS-POLIZA-4 TO WS-POLIZA-4-ANT
      *    MOVE WS-SUBPOL-4 TO WS-SUBPOL-4-ANT

           INITIALIZE RSALIDA
LTA        .

LTA   ******************************************************************10650099
       5000-GRABAR-SALIDA-LTB-P.                                        10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5000-GRABAR-SALIDA-LTB-P'
      *    DISPLAY 'X350JPX3-KPOLIZA        ' X350JPX3-KPOLIZA
      *    DISPLAY 'X350JPX3-KSUBPOL        ' X350JPX3-KSUBPOL
      *F*
           MOVE '3000'                   TO RSAL-MODELO

           MOVE X350JPX3-CCANAL          TO RSAL-CANAL
                                            X350JPX6-CANAL
           MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA
           MOVE X350JPX3-KSUBPOL         TO RSAL-SUBPOLIZA
           MOVE X350JPX3-KSUSCRI         TO RSAL-SUSCRIP

           MOVE X350JPX3-FEFECTO-AINIPOL TO RSAL-FORIGENASUSCRI
           MOVE X350JPX3-FEFECTO-MINIPOL TO RSAL-FORIGENMSUSCRI
           MOVE X350JPX3-FEFECTO-DINIPOL TO RSAL-FORIGENDSUSCRI

           MOVE X350JPX3-KRAMO           TO RSAL-RAMO
                                            X350JPX6-RAMO
           MOVE X350JPX3-MODALI          TO RSAL-MODALIDAD
                                            X350JPX6-MODALIDAD
           MOVE X350JPX3-VIGOR-ANUL      TO RSAL-CESTADOASEG
           MOVE '00'                     TO RSAL-PREGRUPO(1:2)
           MOVE X350JPX3-CARTINV         TO RSAL-PREGRUPO(3:2)
           MOVE X350JPX3-CASADA          TO RSAL-CASADO
           MOVE X350JPX3-PINTER-TECBASE  TO RSAL-I1BTI

      *    MOVE WS-ANNO-DESDE-TRAMO      TO RSAL-FRENO-ADESDE
      *    MOVE WS-MES-DESDE-TRAMO       TO RSAL-FRENO-MDESDE
      *    MOVE WS-DIA-DESDE-TRAMO       TO RSAL-FRENO-DDESDE
PLURIN       MOVE WS-FRENO-DESDE-ABONO-ACTUAL TO RSAL-FRENO-ADESDE
PLURIN       MOVE WS-FRENO-DESDE-MBONO-ACTUAL TO RSAL-FRENO-MDESDE
PLURIN       MOVE WS-FRENO-DESDE-DBONO-ACTUAL TO RSAL-FRENO-DDESDE

           MOVE WS-ANNO-HASTA-TRAMO   TO RSAL-FRENO-AHASTA
           MOVE WS-MES-HASTA-TRAMO    TO RSAL-FRENO-MHASTA
           MOVE WS-DIA-HASTA-TRAMO    TO RSAL-FRENO-DHASTA
           MOVE WS-RENOVADA           TO RSAL-RENOVADA
                                         X350JPX6-RENOVADA

JOSEN      PERFORM CALCULAR-ANNOS-TRANS-MVT-LTP
JOSEN
240524*    PERFORM 3301-RECUPERAR-ALFA-MVT-LTP

      *    IF WS-FECHA-LTA-H         >  R350T000-FFINAL
      *    OR WS-FECHA-LTA-H         =  R350T000-FFINAL
      *       MOVE 'S'                   TO RSAL-RENOVADA
      *                                     X350JPX6-RENOVADA
      *       MOVE 'S' TO WS-RENOVADA
      *    ELSE
      *       MOVE 'N'                   TO RSAL-RENOVADA
      *                                     X350JPX6-RENOVADA
      *       MOVE 'N' TO WS-RENOVADA
      *    END-IF

JOSEN           IF WS-FECHA-HASTA-TRAMO < R350T000-FFINAL
JOSEN              MOVE WS-ANNO-HASTA-TRAMO TO RSAL-FCIERREA
                                                 X350JPX6-FCIERREA
                   MOVE WS-MES-HASTA-TRAMO  TO RSAL-FCIERREM
                                                 X350JPX6-FCIERREM
JOSEN              MOVE WS-DIA-HASTA-TRAMO  TO RSAL-FCIERRED
                                                 X350JPX6-FCIERRED
JOSEN           ELSE
                   MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
                                                 X350JPX6-FCIERREA
                   MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
                                                 X350JPX6-FCIERREM
                   MOVE R350T000-FDFINAL         TO RSAL-FCIERRED
                                                 X350JPX6-FCIERRED
JOSEN           END-IF
      *    MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
      *                                     X350JPX6-FCIERREA
      *    MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
      *                                     X350JPX6-FCIERREM
      *    MOVE R350T000-FDFINAL         TO RSAL-FCIERRED
      *                                     X350JPX6-FCIERRED
           MOVE 'BONO'                   TO RSAL-LTA
           MOVE X350JPX3-FCOBRO          TO RSAL-FORMAPAGO
           MOVE X350JPX3-TIPO-COASE      TO RSAL-COASEGURO
           MOVE X350JPX3-PCOASE          TO RSAL-PCOASEGURO
           MOVE X350JPX3-PAGADOR         TO RSAL-PGLOBAL

021125*    IF X350JXX3-CERTI EQUAL '000000' AND                         000
021125*       X350JXX3-ASEG-MECANIZADOS = 'N'                           000
021125     IF X350JPX3-CERTI EQUAL '000000' AND                         000
021125        X350JPX3-ASEG-MECANIZADOS = 'N'                           000
              MOVE 'S' TO RSAL-IMNOMINADA                               000
           ELSE
              MOVE 'N' TO RSAL-IMNOMINADA                               000
           END-IF

           MOVE X350JPX3-NASEGURADO      TO RSAL-NASEGURADOS

           IF X350JPX3-VIGOR-ANUL  = 'A'
              MOVE X350JPX3-ANO-ANUL     TO RSAL-FECAANULACION
              MOVE X350JPX3-MES-ANUL     TO RSAL-FECMANULACION
              MOVE X350JPX3-DIA-ANUL     TO RSAL-FECDANULACION
              MOVE X350JPX3-MOTIVO-ANU   TO RSAL-MOTIVO
           END-IF


240524          IF WS-IMP-PERI-CAL3 EQUAL ZEROES
240524             MOVE 1 TO WS-ALFA
240524          END-IF



110624*    MOVE WS-ALFA                  TO RSAL-ALFA
110624     MOVE WS-ALFA-PLU-BON          TO RSAL-ALFA
      *D*
      *    DISPLAY 'WS-ALFA-BONO-PLURI' WS-ALFA
      *    DISPLAY 'WS-IMP-PERI-CAL3  ' WS-IMP-PERI-CAL3
      *    DISPLAY 'WS-ALFA-PLU-BON   ' WS-ALFA-PLU-BON
      *F*

130624     MOVE WS-KGRUPEMP-BS-ACTUAL TO X350M004-KGRUPEMP-BS
EMPMAR     IF X350M004-KGRUPEMP-BS = ZEROS
EMPMAR        MOVE SPACES                TO X350M004-KGRUPEMP-BS
EMPMAR     END-IF

           MOVE X350M004-KGRUPEMP-BS(3:3) TO RSAL-GEMPRESA

      *    IF AUX-TIP-REAJUSTE = '1' OR 'T'
      *       MOVE 'S'                      TO RSAL-REAJUSTE
      *    ELSE
      *       MOVE 'N'                      TO RSAL-REAJUSTE
      *    END-IF

           MOVE AUX-CCOBON-REAJUS        TO RSAL-REAJUSTE

           MOVE AUX-REAJUSTE-PB          TO RSAL-REAJUSTE-PB

           MOVE WS-REAJREC               TO RSAL-REAJUSTE-REC
           MOVE AUX-TIP-REAJUSTE         TO RSAL-TIP-REAJUSTE
           MOVE AUX-NANIOPER             TO RSAL-APERDIDAS
           MOVE AUX-NANIOBEN             TO RSAL-ABENEFICI

           IF X350X098-IANTICIPO-S > ZEROES
              MOVE 'S'                   TO RSAL-ANTICIPO-PB
           ELSE
              MOVE 'N'                   TO RSAL-ANTICIPO-PB
           END-IF

           MOVE AUX-PANTICIPO-S          TO RSAL-P-ANTICIPO
           MOVE AUX-CCUADROBAREMO-S      TO RSAL-BAREMO
           MOVE AUX-PPORCEN-REAL         TO RSAL-REAJUSTEO
210524*    MOVE X350M004-PPBBONO-BS      TO RSAL-P-PB-APLICAR
210524*    MOVE X350M004-PPBBONO-BS      TO RSAL-P-REAUSTE-PB
210524*    MOVE X350M004-PPBBONO-BS      TO AUX-PCALCULOPB-S
210524*    MOVE X350M004-PGASBONO-BS     TO RSAL-P-GASTOS-PB
210524     MOVE WS-PRREAPB-BS-ACTUAL     TO RSAL-P-PB-APLICAR
210524     MOVE WS-PRREAPB-BS-ACTUAL     TO RSAL-P-REAUSTE-PB
210524     MOVE WS-PRREAPB-BS-ACTUAL     TO AUX-PCALCULOPB-S
210524     MOVE WS-PGASTOS-BS-ACTUAL     TO RSAL-P-GASTOS-PB

      *D*
      *    DISPLAY 'WS-IMPORTE-PLUGA ' WS-IMPORTE-PLUGA
      *    DISPLAY 'WS-IMPORTE-PLURE ' WS-IMPORTE-PLURE
      *F*
           COMPUTE WS-IMPORTE-LTB = WS-IMPORTE-205 + WS-IMPORTE-PRI

      *D*
      *    DISPLAY 'WS-IMPORTE-205 ' WS-IMPORTE-205
      *F*

      *    COMPUTE RSAL-PRIMA-RECIBOS ROUNDED = WS-IMPORTE-LTB
           MOVE ZEROES TO RSAL-PRIMA-RECIBOS
           MOVE WS-IMPORTE-PLURE         TO RSAL-PRIMA-RECIBOS

           MOVE WS-IMPORTE-PLURE         TO X350JPX6-PRIMA-RECIBOS

           COMPUTE RSAL-APUNTE-RECIBO ROUNDED = WS-IMPORTE-902

           MOVE WS-IMPORTE-902           TO X350JPX6-APUNTE-RECIBO

LTA        COMPUTE WS-IMPORTE-PERIOD-LTA ROUNDED
                   = WS-IMP-PERI-CALC

      *D*
      *         DISPLAY 'WS-IMPORTE-PERIOD-LTA  '  WS-IMPORTE-PERIOD-LTA
      *         DISPLAY 'WS-IMPORTE-PLUPE       '  WS-IMPORTE-PLUPE
      *         DISPLAY 'WS-IMPORTE-PLURE       '  WS-IMPORTE-PLURE
      *F*
           COMPUTE WS-IMPORTE-PERIOD ROUNDED
041124*            = WS-IMP-PERI-CALC
041124             = WS-IMPORTE-PLUPE

      *    COMPUTE RSAL-PRIMA-PERIOD ROUNDED = WS-IMPORTE-PERIOD-LTA

           COMPUTE RSAL-PRIMA-PERIOD ROUNDED = WS-IMPORTE-PLUPE


      *    MOVE WS-IMPORTE-PERIOD        TO X350JPX6-PRIMA-PERIOD
           MOVE WS-IMPORTE-PLUPE         TO X350JPX6-PRIMA-PERIOD

           COMPUTE WS-IMP-GASTOS = (WS-IMPORTE-PLUPE  *
210524*                     X350M004-PGASBONO-BS) / 100
210524                   WS-PGASTOS-BS-ACTUAL) / 100

LTA        COMPUTE WS-IMP-GASTOS-LTA = (WS-IMPORTE-PLUPE *
210524*                     X350M004-PGASBONO-BS) / 100
210524                   WS-PGASTOS-BS-ACTUAL) / 100

      *D*
      *    DISPLAY 'WS-IMPORTE-PLUGA ' WS-IMPORTE-PLUGA
      *F*
           MOVE WS-IMPORTE-PLUGA         TO X350JPX6-GASTOS

      *    COMPUTE RSAL-GASTOS ROUNDED = WS-IMP-GASTOS

110624*    COMPUTE RSAL-GASTOS ROUNDED = WS-IMPORTE-PLUGA
110624     COMPUTE RSAL-GASTOS ROUNDED = WS-IMP-GASTOS-LTA

MF001 *    MOVE WS-IMPORTE-206           TO X350JPX6-TOTAL-SINIES
           MOVE TB-ACUMULADO-206 (5)     TO X350JPX6-TOTAL-SINIES
      *D*
      *    DISPLAY 'WS-IMPORTE-206   ' WS-IMPORTE-206
      *    DISPLAY 'WS-IMPORTE-206-PLU-BON ' WS-IMPORTE-206-PLU-BON
      *    DISPLAY 'WS-IMP-GASTOS          ' WS-IMP-GASTOS
      *    DISPLAY 'WS-IMP-GASTOS-LTA      ' WS-IMP-GASTOS-LTA
      *F*

MF001 *    COMPUTE RSAL-TOTAL-SINIES ROUNDED = WS-IMPORTE-206-PLU-BON
MF001      COMPUTE RSAL-TOTAL-SINIES ROUNDED = TB-ACUMULADO-206 (5)

           MOVE X350X098-IARRASTRE-S     TO RSAL-IMP-ARRAS-PERD

           MOVE WS-PGASTOS-BS-ACTUAL     TO RSAL-P-GASTOS-PB

      **   COMPUTE RSAL-PRIMA-ANUAL ROUNDED =  WS-IMPORTE-PRI
josen      COMPUTE RSAL-PRIMA-ANUAL ROUNDED = X350JXX2-EPNETA

           MOVE WS-IMPORTE-PRI           TO X350JPX6-PRIMA-ANUAL
           MOVE X350X098-IARRASTRE-S TO RSAL-IMP-ARRAS-PERD

           MOVE WS-IMPORTE-PRI           TO X350JPX6-PRIMA-ANUAL
           MOVE X350X098-IARRASTRE-S TO RSAL-IMP-ARRAS-PERD
           MOVE ZEROES                   TO RSAL-NETA-COASEGUR
                                            X350JPX6-NETA-COASEGUR
           MOVE SPACES TO X350JPX5-GEMPRESA

EMPMAR**   IF X350JPX3-KGRUEMP = '000'
EMPMAR     IF X350M004-KGRUPEMP-BS = SPACES
              MOVE SPACES           TO X350JPX3-KGRUEMP
           END-IF

EMPMAR**   IF X350JPX3-KGRUEMP NOT = SPACES
EMPMAR     IF X350M004-KGRUPEMP-BS NOT = SPACES
             MOVE X350JPX4-NASEGURADO    TO RSAL-ASE-TOT-EMPRESA
           END-IF

           MOVE SPACES TO X350JPX5-GEMPRESA

PLURIN          IF WS-RENOVADA = 'N' AND SW-SI-LTA-MITAD
PLURIN
PLURIN          COMPUTE RSAL-PRIMA-PERIOD ROUNDED
PLURIN                  = WS-IMP-PERI-CALC + WS-EPNETA-NO-RENO-PLURI

PLURIN          COMPUTE WS-IMPORTE-PERIOD ROUNDED
PLURIN                  = WS-IMP-PERI-CALC + WS-EPNETA-NO-RENO-PLURI
PLURIN
PLURIN          COMPUTE WS-IMPORTE-PERIOD-LTA ROUNDED
PLURIN                  = WS-IMP-PERI-CALC + WS-EPNETA-NO-RENO-PLURI

0124            COMPUTE RSAL-PRIMA-RECIBOS = X350JXX2-EPNETA
0124                                                 + WS-IMPORTE-205
LTA             COMPUTE WS-IMP-GASTOS-LTA = (WS-IMPORTE-PERIOD-LTA *
210524*                          X350M004-PGASBONO-BS) / 100
210524                           WS-PGASTOS-BS-ACTUAL) / 100
110624          COMPUTE RSAL-GASTOS ROUNDED = WS-IMP-GASTOS-LTA
                END-IF
      *D*
      *        DISPLAY 'X350JPX3-KGRUEMP         ' X350JPX3-KGRUEMP
      *F*

EMPMAR**   IF X350JPX3-KGRUEMP NOT = SPACES
EMPMAR     IF X350M004-KGRUPEMP-BS NOT = SPACES
110624**      OPEN INPUT ENTRADA-TASE-LTP
110624        OPEN INPUT ENTRADA-TASE-LTB
110624**      PERFORM 2703-LEER-TOTALES-LTP
110624        PERFORM 2703-LEER-TOTALES-LTB
EMPMAR**        UNTIL X350JPX3-KGRUEMP EQUAL
EMPMAR          UNTIL X350M004-KGRUPEMP-BS(3:3) EQUAL
                      X350JPX5-GEMPRESA
110624*         CLOSE ENTRADA-TASE-LTP
110624          CLOSE ENTRADA-TASE-LTB

      *D*
      *        DISPLAY 'X350JPX3-KGRUEMP         ' X350JPX3-KGRUEMP
      *        DISPLAY 'X350JPX3-KGRUEMP         ' X350JPX3-KGRUEMP
      *        DISPLAY 'X350JPX5-PRIMA-PERIOD ' X350JPX5-PRIMA-PERIOD
      *F*
              MOVE X350JPX5-PBPAGADA-EMP    TO RSAL-PBPAGA-TOT-EMPR
              MOVE X350JPX5-PRIMA-PERIOD    TO RSAL-PRI-TOT-EMPRESA

              IF X350JPX5-TOTAL-SINIES = ZEROES
                 MOVE AUX-SINIESTROS-LTP    TO RSAL-SIN-TOT-EMPRESA
              ELSE
                 MOVE X350JPX5-TOTAL-SINIES TO RSAL-SIN-TOT-EMPRESA
              END-IF

              COMPUTE RSAL-GASTOS-TOT-EMPR = (X350JPX5-PRIMA-PERIOD *
210524*                        X350M004-PGASBONO-BS) / 100
210524                         WS-PGASTOS-BS-ACTUAL) / 100

              MOVE X350JPX5-PBPAGADA-EMP TO RSAL-PBPAGA-TOT-EMPR
      *D*
      *       DISPLAY 'X350JPX5-PRIMA-PERIOD ' X350JPX5-PRIMA-PERIOD
      *       DISPLAY 'X350JPX5-TOTAL-SINIES ' X350JPX5-TOTAL-SINIES
      *       DISPLAY 'AUX-GASTOS-LTA        ' AUX-GASTOS-LTA
      *       DISPLAY 'WS-IMPORTE-PRI        ' WS-IMPORTE-PRI
      *       DISPLAY 'X350JPX5-GASTOS       ' X350JPX5-GASTOS
      *       DISPLAY 'X350JPX5-PRIMA-ANUAL  ' X350JPX5-PRIMA-ANUAL
      *       DISPLAY 'X350JPX5-PBPAGADA-EMP ' X350JPX5-PBPAGADA-EMP
      *F*

              COMPUTE WS-IMPORTE-REPARTOS = ((X350JPX5-PRIMA-PERIOD -
                                              X350JPX5-TOTAL-SINIES -
                                              X350JPX5-GASTOS   -
                                              X350JPX5-PBPAGADA-EMP) *
                                              X350JXX2-EPNETA) /
                                              X350JPX5-PRIMA-ANUAL

      *D*
      *       DISPLAY 'WS-IMPORTE-REPARTOS ' WS-IMPORTE-REPARTOS
      *F*
              MOVE WS-IMPORTE-REPARTOS TO X350JPX6-INGRESOS-GASTOS

              IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
                 MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
              END-IF

               COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
                                         = WS-IMPORTE-REPARTOS

               COMPUTE WS-NETA-COASEGUR ROUNDED
                                         = (WS-IMPORTE-REPARTOS *
                                            X350JPX3-PCOASE) / 100
                MOVE WS-NETA-COASEGUR         TO X350JPX6-NETA-COASEGUR

                IF X350JPX6-NETA-COASEGUR NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-NETA-COASEGUR
                END-IF

                COMPUTE RSAL-NETA-COASEGUR ROUNDED
                                           = WS-NETA-COASEGUR

                MOVE RSAL-P-PB-APLICAR  TO AUX-PB-TECNICA

                COMPUTE RSAL-PB-TECNICA ROUNDED
                                        = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100
                COMPUTE X350JPX6-PB-TECNICA = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100

                IF X350JPX6-PB-TECNICA NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-PB-TECNICA
                END-IF

                MOVE X350JPX3-PCOASE          TO AUX-PCOASE

                COMPUTE X350JPX6-DEV-NETCOASE = (WS-NETA-COASEGUR *
210524*                                X350M004-PPBBONO-BS) / 100
210524                              WS-PRREAPB-BS-ACTUAL) / 100

      *D*
      *         DISPLAY 'WS-NETA-COASEGUR ' WS-NETA-COASEGUR
      *         DISPLAY 'X350M004-PPBBONO-BS ' X350M004-PPBBONO-BS
      *F*
                IF X350JPX6-DEV-NETCOASE < ZEROES
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                   MOVE ZEROES TO RSAL-DEV-NETCOASE
                ELSE
                   COMPUTE RSAL-DEV-NETCOASE ROUNDED
                                          = (WS-NETA-COASEGUR *
210524*                                X350M004-PPBBONO-BS) / 100
210524                              WS-PRREAPB-BS-ACTUAL) / 100
      *D*
      *         DISPLAY 'RSAL-DEV-NETCOASE ' RSAL-DEV-NETCOASE
      *F*
                END-IF

                IF X350JPX6-DEV-NETCOASE NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                END-IF


           ELSE
      *D*
      *        DISPLAY 'ME VOY POR ELSE SI LTB-P'
      *        DISPLAY 'X350JPX4-KPOLIZA       ' X350JPX4-KPOLIZA
      *        DISPLAY 'X350JPX3-KPOLIZA       ' X350JPX3-KPOLIZA
      *        DISPLAY 'X350JPX3-KSUBPOL       ' X350JPX3-KSUBPOL
      *        DISPLAY 'X350JPX4-KSUBPOL       ' X350JPX4-KSUBPOL
      *        DISPLAY 'X350JPX3-KGRUEMP       ' X350JPX3-KGRUEMP
      *        DISPLAY 'X350JPX4-KGRUEMP       ' X350JPX4-KGRUEMP
      *F*
               OPEN INPUT ENTRADA-TASE-NE
                  PERFORM 2703-LEER-TOTALES-ASEGURADO-NE
                    UNTIL X350JPX3-KPOLIZA EQUAL
021124**                  X350JPX5-KPOLIZA AND
021124                    X350JPX4-KPOLIZA AND
                          X350JPX3-KSUBPOL EQUAL
                          X350JPX4-KSUBPOL AND
EMPMAR**                  X350JPX3-KGRUEMP EQUAL
EMPMAR                    X350M004-KGRUPEMP-BS(3:3) =
                          X350JPX4-KGRUEMP
                    CLOSE ENTRADA-TASE-NE

               OPEN INPUT ENTRADA-TASE-CP
                  PERFORM 2703-LEER-TOTALES-ASEGURADO-CP
                    UNTIL X350JPX3-KPOLIZA EQUAL
                          X350JPX5-KPOLIZA AND
                          X350JPX3-KSUBPOL EQUAL
                          X350JPX5-KSUBPOL
                    CLOSE ENTRADA-TASE-CP

      *D*
      *        DISPLAY 'X350JPX5-PRIMA-ANUAL '  X350JPX5-PRIMA-ANUAL
      *F*
               IF X350JPX5-PRIMA-ANUAL <= ZEROES
                  MOVE ZEROES TO WS-IMPORTE-REPARTOS
               ELSE
                  COMPUTE WS-IMPORTE-REPARTOS = ((WS-IMPORTE-PERIOD -
MF001 *                                         WS-IMPORTE-206    -
MF001                                           TB-ACUMULADO-206 (5) -
                                                WS-IMP-GASTOS) *
                                                X350JPX5-PRIMA-ANUAL) /
                                                X350JPX5-PRIMA-ANUAL

               END-IF

      *D*
      *        DISPLAY 'WS-IMPORTE-REPARTOS  '  WS-IMPORTE-REPARTOS
      *F*
                   MOVE WS-IMPORTE-REPARTOS TO X350JPX6-INGRESOS-GASTOS

                   IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
                      MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
                   END-IF

                   COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
                                                = WS-IMPORTE-REPARTOS


                COMPUTE WS-NETA-COASEGUR ROUNDED
                                         = (WS-IMPORTE-REPARTOS *
                                            X350JPX3-PCOASE) / 100
                MOVE WS-NETA-COASEGUR         TO X350JPX6-NETA-COASEGUR

                IF X350JPX6-NETA-COASEGUR NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-NETA-COASEGUR
                END-IF

                COMPUTE RSAL-NETA-COASEGUR ROUNDED
                                           = WS-NETA-COASEGUR

                MOVE RSAL-P-PB-APLICAR  TO AUX-PB-TECNICA

      *         COMPUTE RSAL-PB-TECNICA ROUNDED
      *                                 = (WS-IMPORTE-REPARTOS *
      *                                    AUX-PB-TECNICA) / 100
                COMPUTE X350JPX6-PB-TECNICA = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100

                IF X350JPX6-PB-TECNICA NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-PB-TECNICA
                END-IF

                MOVE X350JPX3-PCOASE          TO AUX-PCOASE

                COMPUTE X350JPX6-DEV-NETCOASE = (WS-NETA-COASEGUR *
                                       AUX-PB-TECNICA) / 100

                IF X350JPX6-DEV-NETCOASE < ZEROES
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                   MOVE ZEROES TO RSAL-DEV-NETCOASE
                ELSE
                   COMPUTE RSAL-DEV-NETCOASE ROUNDED
                                          = (WS-NETA-COASEGUR *
                                          AUX-PB-TECNICA) / 100
                END-IF

                IF X350JPX6-DEV-NETCOASE NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                END-IF

      *
      *
      *       COMPUTE WS-IMPORTE-REPARTOS = ((X350JPX5-PRIMA-PERIOD -
      *                                       WS-IMPORTE-206    -
200524*                                       AUX-GASTOS-LTA) *
200524*                                       X350JPX5-GASTOS) *
      *                                       X350JPX5-PRIMA-ANUAL) /
      *                                       X350JPX5-PRIMA-ANUAL
      *
      *       MOVE WS-IMPORTE-REPARTOS TO X350JPX6-INGRESOS-GASTOS
      *
      *       IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
      *          MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
      *       END-IF
      *
      *           COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
      *                                        = WS-IMPORTE-REPARTOS
      *
      *
      *         COMPUTE WS-NETA-COASEGUR ROUNDED
      *                                  = (WS-IMPORTE-REPARTOS *
      *                                     X350JPX3-PCOASE) / 100
      *         MOVE WS-NETA-COASEGUR         TO X350JPX6-NETA-COASEGUR
      *
      *         IF X350JPX6-NETA-COASEGUR NOT NUMERIC
      *            MOVE ZEROES TO X350JPX6-NETA-COASEGUR
      *         END-IF
      *
      *         COMPUTE RSAL-NETA-COASEGUR ROUNDED
      *                                    = WS-NETA-COASEGUR
      *
      *         MOVE RSAL-P-PB-APLICAR  TO AUX-PB-TECNICA
      *
      *         COMPUTE RSAL-PB-TECNICA ROUNDED
      *                                 = (WS-IMPORTE-REPARTOS *
      *                                    AUX-PB-TECNICA) / 100
      *         COMPUTE X350JPX6-PB-TECNICA = (WS-IMPORTE-REPARTOS *
      *                                    AUX-PB-TECNICA) / 100
      *
      *         IF X350JPX6-PB-TECNICA NOT NUMERIC
      *            MOVE ZEROES TO X350JPX6-PB-TECNICA
      *         END-IF
      *
      *         MOVE X350JPX3-PCOASE          TO AUX-PCOASE
      *
      *         COMPUTE X350JPX6-DEV-NETCOASE = (WS-NETA-COASEGUR *
      *                                AUX-PB-TECNICA) / 100
      *
      *         IF X350JPX6-DEV-NETCOASE < ZEROES
      *            MOVE ZEROES TO X350JPX6-DEV-NETCOASE
      *            MOVE ZEROES TO RSAL-DEV-NETCOASE
      *         ELSE
      *            COMPUTE RSAL-DEV-NETCOASE ROUNDED
      *                                   = (WS-NETA-COASEGUR *
      *                                   AUX-PB-TECNICA) / 100
      *         END-IF
      *
      *         IF X350JPX6-DEV-NETCOASE NOT NUMERIC
      *            MOVE ZEROES TO X350JPX6-DEV-NETCOASE
      *         END-IF

PBTEC      END-IF

           IF RSAL-PGLOBAL = '0'
              MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA-REF1
              MOVE '/'                      TO RSAL-SEPARADOR
              MOVE '0000'                   TO RSAL-SUBPOL-REF1
           END-IF

           IF RSAL-POLIZA-REF1 = '0000000'
              MOVE SPACES TO RSAL-POLIZA-REF1
              MOVE SPACES TO RSAL-SEPARADOR
              MOVE SPACES TO RSAL-SUBPOL-REF1
           END-IF



           PERFORM 2210-RECUPERAR-UOA-NR
           PERFORM 2300-OBTENER-NEW-GAP

           IF RSAL-REAJUSTE = 'S' AND  RSAL-REAJUSTE-PB = SPACES
              MOVE 'POL CON REAJUSTE CON SU TIPO PENDIENTE'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'B350JA12'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE '01'                TO X350J000-ID-ERROR-MOD
              PERFORM 2400-ESCRIBIR-ERRORES
           ELSE
              IF RSAL-REAJUSTE = 'S' AND
                 RSAL-REAJUSTE-PB NOT = SPACES
                 MOVE 'POL CON REAJUSTE CON SU TIPO INCONGRUENTE'
                                           TO X350J000-DESC-ERROR
                  MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                  MOVE 'B350JA12'          TO X350J000-ID-MODULO
                  MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                  MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                  MOVE 'W'                 TO X350J000-NIVEL
                  MOVE '02'                TO X350J000-ID-ERROR-MOD
                  PERFORM 2400-ESCRIBIR-ERRORES
                END-IF
           END-IF

           IF RSAL-REAJUSTE = 'N'           AND
              RSAL-REAJUSTE-PB NOT = SPACES OR
              AUX-PCALCULOPB-S NOT = ZEROES OR
              AUX-PPORCEN-REAL NOT = ZEROES
              MOVE 'POL SIN REAJUSTE PERO CON DATOS DEL MISMO'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'B350JA12'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE '03'                TO X350J000-ID-ERROR-MOD

              PERFORM 2400-ESCRIBIR-ERRORES
           END-IF

      *    PERFORM 5214-CALCULOS-LTB

110624*    MOVE WS-IMPORTE-PBP         TO RSAL-PBPAGA
311224     IF SW-SI-LTA-FINAL
PLURIN        MOVE ZEROES TO RSAL-PBPAGA
PLURIN     ELSE
170624        MOVE WS-IMPORTE-PBP-BONO    TO RSAL-PBPAGA
PLURIN     END-IF

      *D*
      *          DISPLAY 'RSAL-FECHA CIERRE: ' WS-FECSIN-CIERRE
      *          DISPLAY 'RSAL-FECHA HASTA: ' WS-FECSIN-HASTA
      *F*
LUNA       MOVE RSAL-FCIERREA TO WS-FECSIN-ACIERRE
LUNA       MOVE RSAL-FCIERREM TO WS-FECSIN-MCIERRE
LUNA       MOVE RSAL-FCIERRED TO WS-FECSIN-DCIERRE
LUNA
LUNA       MOVE WS-FECSIN-DESDE-PLU-BON TO RSAL-FECSIN-DESDE
LUNA       IF WS-FECSIN-CIERRE < WS-FECSIN-HASTA-PLU-BON
LUNA          MOVE WS-FECSIN-CIERRE TO RSAL-FECSIN-HASTA
LUNA       ELSE
LUNA          MOVE WS-FECSIN-HASTA-PLU-BON TO RSAL-FECSIN-HASTA
LUNA       END-IF

           WRITE REG-SALIDA1 FROM RSALIDA

           MOVE ZEROES TO WS-IMPORTE-PERIOD-LTA

      *    MOVE WS-POLIZA-2 TO WS-POLIZA-2-ANT
      *    MOVE WS-SUBPOL-2 TO WS-SUBPOL-2-ANT
      *    MOVE WS-POLIZA-4 TO WS-POLIZA-4-ANT
      *    MOVE WS-SUBPOL-4 TO WS-SUBPOL-4-ANT

           INITIALIZE RSALIDA
LTA        .

LTA   ******************************************************************10650099
       5000-GRABAR-SALIDA-LTB-NR.                                       10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5000-GRABAR-SALIDA-LTB-NR'
      *    DISPLAY 'X350JPX3-KPOLIZA         ' X350JPX3-KPOLIZA
      *    DISPLAY 'X350JPX3-KSUBPOL         ' X350JPX3-KSUBPOL
      *F*
           MOVE '3000'                   TO RSAL-MODELO

           MOVE X350JPX3-CCANAL          TO RSAL-CANAL
                                            X350JPX6-CANAL
           MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA
           MOVE X350JPX3-KSUBPOL         TO RSAL-SUBPOLIZA
           MOVE X350JPX3-KSUSCRI         TO RSAL-SUSCRIP

           MOVE X350JPX3-FEFECTO-AINIPOL TO RSAL-FORIGENASUSCRI
           MOVE X350JPX3-FEFECTO-MINIPOL TO RSAL-FORIGENMSUSCRI
           MOVE X350JPX3-FEFECTO-DINIPOL TO RSAL-FORIGENDSUSCRI

           MOVE X350JPX3-KRAMO           TO RSAL-RAMO
                                            X350JPX6-RAMO
           MOVE X350JPX3-MODALI          TO RSAL-MODALIDAD
                                            X350JPX6-MODALIDAD
           MOVE X350JPX3-VIGOR-ANUL      TO RSAL-CESTADOASEG
           MOVE '00'                     TO RSAL-PREGRUPO(1:2)
           MOVE X350JPX3-CARTINV         TO RSAL-PREGRUPO(3:2)
           MOVE X350JPX3-CASADA          TO RSAL-CASADO
           MOVE X350JPX3-PINTER-TECBASE  TO RSAL-I1BTI

1412***    MOVE WS-ANNO-DESDE-TRAMO      TO RSAL-FRENO-ADESDE
1412***    MOVE WS-MES-DESDE-TRAMO       TO RSAL-FRENO-MDESDE
1412***    MOVE WS-DIA-DESDE-TRAMO       TO RSAL-FRENO-DDESDE

1412***    MOVE WS-ANNO-HASTA-TRAMO   TO RSAL-FRENO-AHASTA
1412***    MOVE WS-MES-HASTA-TRAMO    TO RSAL-FRENO-MHASTA
1412***    MOVE WS-DIA-HASTA-TRAMO    TO RSAL-FRENO-DHASTA
1412       MOVE WS-FRENO-DESDE-ABONO-ACTUAL TO RSAL-FRENO-ADESDE
1412       MOVE WS-FRENO-DESDE-MBONO-ACTUAL TO RSAL-FRENO-MDESDE
1412       MOVE WS-FRENO-DESDE-DBONO-ACTUAL TO RSAL-FRENO-DDESDE

1412       MOVE X350M004-FHASA-BS           TO RSAL-FRENO-AHASTA
1412       MOVE X350M004-FHASM-BS           TO RSAL-FRENO-MHASTA
1412       MOVE X350M004-FHASD-BS           TO RSAL-FRENO-DHASTA

           MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
                                            X350JPX6-FCIERREA
           MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
                                            X350JPX6-FCIERREM
           MOVE R350T000-FDFINAL         TO RSAL-FCIERRED
                                            X350JPX6-FCIERRED
           MOVE WS-RENOVADA              TO RSAL-RENOVADA
                                            X350JPX6-RENOVADA

           MOVE 'BONO'                   TO RSAL-LTA
           MOVE X350JXX2-FCOBRO          TO RSAL-FORMAPAGO
           MOVE X350JPX3-TIPO-COASE      TO RSAL-COASEGURO
           MOVE X350JPX3-PCOASE          TO RSAL-PCOASEGURO
           MOVE X350JPX3-PAGADOR         TO RSAL-PGLOBAL

021125*    IF X350JXX3-CERTI EQUAL '000000' AND                         000
021125*       X350JXX3-ASEG-MECANIZADOS = 'N'                           000
021125     IF X350JPX3-CERTI EQUAL '000000' AND                         000
021125        X350JPX3-ASEG-MECANIZADOS = 'N'                           000
              MOVE 'S' TO RSAL-IMNOMINADA                               000
           ELSE
              MOVE 'N' TO RSAL-IMNOMINADA                               000
           END-IF

           MOVE X350JPX3-NASEGURADO      TO RSAL-NASEGURADOS

           IF X350JPX3-VIGOR-ANUL  = 'A'
              MOVE X350JPX3-ANO-ANUL     TO RSAL-FECAANULACION
              MOVE X350JPX3-MES-ANUL     TO RSAL-FECMANULACION
              MOVE X350JPX3-DIA-ANUL     TO RSAL-FECDANULACION
              MOVE X350JPX3-MOTIVO-ANU   TO RSAL-MOTIVO
           END-IF


130924     IF WS-FECHA-NR > WS-FECHA-HASTA-TRAMO
130924          MOVE 1                   TO RSAL-ALFA
130924     ELSE
                MOVE WS-ALFA             TO RSAL-ALFA
130924     END-IF

130624     MOVE WS-KGRUPEMP-BS-ACTUAL TO X350M004-KGRUPEMP-BS
EMPMAR     IF X350M004-KGRUPEMP-BS = ZEROS
EMPMAR        MOVE SPACES                TO X350M004-KGRUPEMP-BS
EMPMAR     END-IF

           MOVE X350M004-KGRUPEMP-BS(3:3) TO RSAL-GEMPRESA

           MOVE AUX-CCOBON-REAJUS        TO RSAL-REAJUSTE

           MOVE AUX-REAJUSTE-PB          TO RSAL-REAJUSTE-PB

           MOVE WS-REAJREC               TO RSAL-REAJUSTE-REC
           MOVE AUX-TIP-REAJUSTE         TO RSAL-TIP-REAJUSTE
           MOVE AUX-NANIOPER             TO RSAL-APERDIDAS
           MOVE AUX-NANIOBEN             TO RSAL-ABENEFICI

           IF X350X098-IANTICIPO-S > ZEROES
              MOVE 'S'                   TO RSAL-ANTICIPO-PB
           ELSE
              MOVE 'N'                   TO RSAL-ANTICIPO-PB
           END-IF

           MOVE AUX-PANTICIPO-S          TO RSAL-P-ANTICIPO
           MOVE AUX-CCUADROBAREMO-S      TO RSAL-BAREMO
           MOVE AUX-PPORCEN-REAL         TO RSAL-REAJUSTEO
210524*    MOVE X350M004-PPBBONO-BS      TO RSAL-P-PB-APLICAR
210524*    MOVE X350M004-PPBBONO-BS      TO RSAL-P-REAUSTE-PB
210524*    MOVE X350M004-PPBBONO-BS      TO AUX-PCALCULOPB-S
210524*    MOVE X350M004-PGASBONO-BS     TO RSAL-P-GASTOS-PB
210524     MOVE WS-PRREAPB-BS-ACTUAL     TO RSAL-P-PB-APLICAR
210524     MOVE WS-PRREAPB-BS-ACTUAL     TO RSAL-P-REAUSTE-PB
210524     MOVE WS-PRREAPB-BS-ACTUAL     TO AUX-PCALCULOPB-S
210524     MOVE WS-PGASTOS-BS-ACTUAL     TO RSAL-P-GASTOS-PB

      *D*
      *    DISPLAY 'BONO NO RENO   '
      *    DISPLAY 'WS-IMPORTE-205 ' WS-IMPORTE-205
      *    DISPLAY 'WS-IMPORTE-PRI ' WS-IMPORTE-PRI
      *    DISPLAY 'WS-IMPORTE-205-BONO ' WS-IMPORTE-205-BONO
      *    DISPLAY 'WS-FECHA-NR         ' WS-FECHA-NR
      *    DISPLAY 'WS-FECHA-HASTA-TRAMO' WS-FECHA-HASTA-TRAMO
      *F*
           COMPUTE WS-IMPORTE-LTB = WS-IMPORTE-205 + WS-IMPORTE-PRI

      *D*
      *    DISPLAY 'WS-IMPORTE-LTB ' WS-IMPORTE-LTB
      *F*

           MOVE ZEROES TO RSAL-PRIMA-RECIBOS

              IF WS-FECHA-NR > WS-FECHA-HASTA-TRAMO

130824*    COMPUTE RSAL-PRIMA-RECIBOS = WS-IMPORTE-205
020924        COMPUTE RSAL-PRIMA-RECIBOS = WS-IMPORTE-205-BONO-NO-RENO
130824                                   + WS-IMPORTE-205
020924*                                  + X350JPX3-EPNETA
           COMPUTE RSAL-APUNTE-RECIBO = WS-IMPORTE-902
                                      + WS-IMPORTE-902-BONO
              ELSE

020924        COMPUTE RSAL-PRIMA-RECIBOS = WS-IMPORTE-205-BONO-NO-RENO
130824*                                  + WS-IMPORTE-205
0124                                     + WS-IMPORTE-205
020924                                   + X350JPX3-EPNETA

              COMPUTE RSAL-APUNTE-RECIBO = WS-IMPORTE-902
                                      + WS-IMPORTE-902-BONO
              END-IF

100124     IF SW-SI-LTA-FINAL
              COMPUTE RSAL-PRIMA-RECIBOS ROUNDED = X350JPX3-EPNETA
           END-IF

           MOVE WS-IMPORTE-205           TO X350JPX6-PRIMA-RECIBOS

           COMPUTE RSAL-APUNTE-RECIBO ROUNDED = WS-IMPORTE-902
                                              + WS-IMPORTE-902-BONO

           MOVE WS-IMPORTE-902           TO X350JPX6-APUNTE-RECIBO


      *D*
      *         DISPLAY ' BONO-NO-RENO-JOSE     '
      *         DISPLAY ' WS-FECHA-NR           ' WS-FECHA-NR
      *         DISPLAY ' BONO-NO-RENO-JOSE     ' WS-FECHA-HASTA-TRAMO
      *         DISPLAY ' WS-IMPORTE-902 - BONO '  WS-IMPORTE-902
      *         DISPLAY ' WS-IMPORTE-205        '  WS-IMPORTE-205
      *         DISPLAY ' WS-IMPORTE-205 - BONO '  WS-IMPORTE-205-BONO
      *         DISPLAY ' 205-BONO-NO-RENO ' WS-IMPORTE-205-BONO-NO-RENO
      *         DISPLAY ' X350JPX3-EPNETA       '  X350JPX3-EPNETA
      *F*

PLURIN     IF SW-SI-LTA-FINAL
PLURIN
PLURIN        COMPUTE WS-IMPORTE-PERIOD-LTA ROUNDED
PLURIN                = WS-IMPORTE-PERIOD-BONO

PLURIN     ELSE
0124          COMPUTE WS-IMPORTE-PERIOD-LTA ROUNDED
0124                  = (WS-IMPORTE-205 + WS-IMPORTE-902) *
0124                     WS-ALFA + WS-IMPORTE-205-BONO +
0124                                     WS-IMPORTE-902-BONO
PLURIN     END-IF
      *D*
      *         DISPLAY ' WS-IMPORTE-PERIOD-LTA '  WS-IMPORTE-PERIOD-LTA
      *F*
PBTEC
           COMPUTE WS-IMPORTE-PERIOD ROUNDED
                   = (WS-IMPORTE-LTB +
                      WS-IMPORTE-902)

           COMPUTE RSAL-PRIMA-PERIOD ROUNDED = WS-IMPORTE-PERIOD-LTA


           MOVE WS-IMPORTE-PERIOD        TO X350JPX6-PRIMA-PERIOD

           COMPUTE WS-IMP-GASTOS = (WS-IMPORTE-PERIOD *
210524*                     X350M004-PGASBONO-BS) / 100
210524                   WS-PGASTOS-BS-ACTUAL) / 100

LTA        COMPUTE WS-IMP-GASTOS-LTA = (WS-IMPORTE-PERIOD-LTA *
210524*                     X350M004-PGASBONO-BS) / 100
210524                   WS-PGASTOS-BS-ACTUAL) / 100

      *D*
      *    DISPLAY 'WS-IMP-GASTOS-LTA ' WS-IMP-GASTOS-LTA
      *F*
           MOVE WS-IMP-GASTOS            TO X350JPX6-GASTOS

JOSEN           COMPUTE WS-IMPORTE-205-BONO = (WS-IMPORTE-205    +
JOSEN                                   WS-IMPORTE-PERIOD)

           COMPUTE RSAL-GASTOS ROUNDED = WS-IMP-GASTOS-LTA
                IF SW-SI-LTBA
                   CONTINUE
                ELSE
                   MOVE WS-IMPORTE-206-SINI TO WS-IMPORTE-206
                END-IF
      *D*
      *         DISPLAY 'WS-IMPORTE-205-BONO: ' WS-IMPORTE-205-BONO
      *         DISPLAY 'WS-IMPORTE-PERIOD  : ' WS-IMPORTE-PERIOD
      *         DISPLAY 'WS-IMPORTE-206 ' WS-IMPORTE-206
      *F*

MF001 *    MOVE WS-IMPORTE-206           TO X350JPX6-TOTAL-SINIES
           MOVE TB-ACUMULADO-206 (5)     TO X350JPX6-TOTAL-SINIES
      *D*
      *    DISPLAY 'WS-IMPORTE-206    ' WS-IMPORTE-206
      *F*

MF001 *    COMPUTE RSAL-TOTAL-SINIES ROUNDED = WS-IMPORTE-206
           COMPUTE RSAL-TOTAL-SINIES ROUNDED = TB-ACUMULADO-206 (5)

           MOVE X350X098-IARRASTRE-S     TO RSAL-IMP-ARRAS-PERD

      *    MOVE X350M004-PGASBONO-BS     TO RSAL-P-GASTOS-PB

      *    COMPUTE RSAL-PRIMA-ANUAL ROUNDED =  WS-IMPORTE-PRI
josen *    COMPUTE RSAL-PRIMA-ANUAL ROUNDED = X350JXX2-EPNETA
200524     COMPUTE RSAL-PRIMA-ANUAL ROUNDED = X350JPX3-EPNETA
      *D*
      *    DISPLAY 'X350JPX3-EPNETA   ' X350JPX3-EPNETA
      *    DISPLAY 'X350JXX2-EPNETA   ' X350JXX2-EPNETA
      *F*

200524*    MOVE WS-IMPORTE-PRI           TO X350JPX6-PRIMA-ANUAL
200524     MOVE X350JPX3-EPNETA          TO X350JPX6-PRIMA-ANUAL
           MOVE X350X098-IARRASTRE-S TO RSAL-IMP-ARRAS-PERD

200524*    MOVE WS-IMPORTE-PRI           TO X350JPX6-PRIMA-ANUAL
200524     MOVE X350JPX3-EPNETA          TO X350JPX6-PRIMA-ANUAL
           MOVE X350X098-IARRASTRE-S TO RSAL-IMP-ARRAS-PERD
           MOVE ZEROES                   TO RSAL-NETA-COASEGUR
                                            X350JPX6-NETA-COASEGUR

EMPMAR**   IF X350JPX3-KGRUEMP = '000'
EMPMAR     IF X350M004-KGRUPEMP-BS = SPACES
              MOVE SPACES           TO X350JPX3-KGRUEMP
           END-IF

EMPMAR**   IF X350JPX3-KGRUEMP NOT = SPACES
EMPMAR     IF X350M004-KGRUPEMP-BS NOT = SPACES
             MOVE X350JPX4-NASEGURADO    TO RSAL-ASE-TOT-EMPRESA
           END-IF

           MOVE SPACES TO X350JPX5-GEMPRESA

EMPMAR**   IF X350JPX3-KGRUEMP NOT = SPACES
EMPMAR     IF X350M004-KGRUPEMP-BS NOT = SPACES
              OPEN INPUT ENTRADA-TASE-LTA
              PERFORM 2703-LEER-TOTALES-LTA
EMPMAR**        UNTIL X350JPX3-KGRUEMP EQUAL
                UNTIL X350M004-KGRUPEMP-BS(3:3) EQUAL
                      X350JPX5-GEMPRESA
                CLOSE ENTRADA-TASE-LTA

              SET SW-NO-FINAL-TASE-LTA TO TRUE
      *D*
      *        DISPLAY 'X350JPX3-KGRUEMP         ' X350JPX3-KGRUEMP
      *        DISPLAY 'X350JPX5-PRIMA-PERIOD ' X350JPX5-PRIMA-PERIOD
      *        DISPLAY 'X350JPX5-PBPAGADA-EMP ' X350JPX5-PBPAGADA-EMP
      *F*

              MOVE X350JPX5-PRIMA-PERIOD TO RSAL-PRI-TOT-EMPRESA
              MOVE X350JPX5-TOTAL-SINIES TO RSAL-SIN-TOT-EMPRESA

              COMPUTE RSAL-GASTOS-TOT-EMPR = (X350JPX5-PRIMA-PERIOD *
210524*                     X350M004-PGASBONO-BS) / 100
210524                   WS-PGASTOS-BS-ACTUAL) / 100

              MOVE X350JPX5-PBPAGADA-EMP TO RSAL-PBPAGA-TOT-EMPR
      *D*
      *       DISPLAY 'bono  '
      *       DISPLAY 'X350JPX5-PRIMA-PERIOD ' X350JPX5-PRIMA-PERIOD
      *       DISPLAY 'X350JPX5-TOTAL-SINIES ' X350JPX5-TOTAL-SINIES
      *       DISPLAY 'X350JPX5-GASTOS       ' X350JPX5-GASTOS
      *       DISPLAY 'X350JPX5-PBPAGADA-EMP ' X350JPX5-PBPAGADA-EMP
      *       DISPLAY 'WS-IMPORTE-PRI        ' WS-IMPORTE-PRI
      *       DISPLAY 'X350JPX5-PRIMA-ANUAL  ' X350JPX5-PRIMA-ANUAL
      *F*

270524         IF X350JPX5-PRIMA-ANUAL <= ZEROES
270524            MOVE ZEROES TO WS-IMPORTE-REPARTOS
270524         ELSE

              COMPUTE WS-IMPORTE-REPARTOS = ((X350JPX5-PRIMA-PERIOD -
                                             X350JPX5-TOTAL-SINIES  -
                                             X350JPX5-GASTOS   -
                                             X350JPX5-PBPAGADA-EMP) *
300524*                                      X350JXX2-EPNETA) /
300524                                       X350JPX3-EPNETA) /
                                             X350JPX5-PRIMA-ANUAL

      *D*
      *       DISPLAY 'WS-IMPORTE-REPARTOS ' WS-IMPORTE-REPARTOS
      *F*
              MOVE WS-IMPORTE-REPARTOS TO X350JPX6-INGRESOS-GASTOS

              IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
                 MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
              END-IF

               COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
                                         = WS-IMPORTE-REPARTOS

               COMPUTE WS-NETA-COASEGUR ROUNDED
                                         = (WS-IMPORTE-REPARTOS *
                                            X350JPX3-PCOASE) / 100
                MOVE WS-NETA-COASEGUR         TO X350JPX6-NETA-COASEGUR

                IF X350JPX6-NETA-COASEGUR NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-NETA-COASEGUR
                END-IF

                COMPUTE RSAL-NETA-COASEGUR ROUNDED
                                           = WS-NETA-COASEGUR

                MOVE RSAL-P-PB-APLICAR  TO AUX-PB-TECNICA

                COMPUTE RSAL-PB-TECNICA ROUNDED
                                        = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100
                COMPUTE X350JPX6-PB-TECNICA = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100

                IF X350JPX6-PB-TECNICA NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-PB-TECNICA
                END-IF

                MOVE X350JPX3-PCOASE          TO AUX-PCOASE

                COMPUTE X350JPX6-DEV-NETCOASE = (WS-NETA-COASEGUR *
210524*                                X350M004-PPBBONO-BS) / 100
210524                              WS-PRREAPB-BS-ACTUAL) / 100

      *D*
      *         DISPLAY 'WS-NETA-COASEGUR ' WS-NETA-COASEGUR
      *         DISPLAY 'X350M004-PPBBONO-BS ' X350M004-PPBBONO-BS
      *F*
                IF X350JPX6-DEV-NETCOASE < ZEROES
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                   MOVE ZEROES TO RSAL-DEV-NETCOASE
                ELSE
                   COMPUTE RSAL-DEV-NETCOASE ROUNDED
                                          = (WS-NETA-COASEGUR *
210524*                                X350M004-PPBBONO-BS) / 100
210524                              WS-PRREAPB-BS-ACTUAL) / 100
      *D*
      *         DISPLAY 'RSAL-DEV-NETCOASE ' RSAL-DEV-NETCOASE
      *F*
                END-IF

                IF X350JPX6-DEV-NETCOASE NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                END-IF
270524        END-IF

           ELSE
      *D*
      *        DISPLAY 'ME VOY POR ELSE SI BONO-NR'
      *        DISPLAY 'X350JPX4-KPOLIZA       ' X350JPX4-KPOLIZA
      *        DISPLAY 'X350JPX3-KPOLIZA       ' X350JPX3-KPOLIZA
      *        DISPLAY 'X350JPX3-KSUBPOL       ' X350JPX3-KSUBPOL
      *        DISPLAY 'X350JPX4-KSUBPOL       ' X350JPX4-KSUBPOL
      *        DISPLAY 'X350JPX3-KGRUEMP       ' X350JPX3-KGRUEMP
      *        DISPLAY 'X350JPX4-KGRUEMP       ' X350JPX4-KGRUEMP
      *F*
               OPEN INPUT ENTRADA-TASE-NE
                  PERFORM 2703-LEER-TOTALES-ASEGURADO-NE
                    UNTIL X350JPX3-KPOLIZA EQUAL
EMPMAR**                  X350JPX5-KPOLIZA AND
EMPMAR                    X350JPX4-KPOLIZA AND
                          X350JPX3-KSUBPOL EQUAL
                          X350JPX4-KSUBPOL AND
EMPMAR**                  X350JPX3-KGRUEMP EQUAL
EMPMAR                    X350M004-KGRUPEMP-BS(3:3) EQUAL
                          X350JPX4-KGRUEMP
                    CLOSE ENTRADA-TASE-NE

               OPEN INPUT ENTRADA-TASE-CP
                  PERFORM 2703-LEER-TOTALES-ASEGURADO-CP
                    UNTIL X350JPX3-KPOLIZA EQUAL
                          X350JPX5-KPOLIZA AND
                          X350JPX3-KSUBPOL EQUAL
                          X350JPX5-KSUBPOL
                    CLOSE ENTRADA-TASE-CP

      *D*
      *        DISPLAY 'X350JPX5-KPOLIZA     '  X350JPX5-KPOLIZA
      *        DISPLAY 'X350JPX4-KPOLIZA     '  X350JPX4-KPOLIZA
      *        DISPLAY 'X350JPX5-KSUBPOL     '  X350JPX5-KSUBPOL
      *        DISPLAY 'X350JPX4-KSUBPOL     '  X350JPX4-KSUBPOL
      *        DISPLAY 'X350JPX5-PRIMA-ANUAL '  X350JPX5-PRIMA-ANUAL
      *        DISPLAY 'X350JPX4-PRIMA-ANUAL '  X350JPX5-PRIMA-ANUAL
      *F*
               IF X350JPX5-PRIMA-ANUAL <= ZEROES
                  MOVE ZEROES TO WS-IMPORTE-REPARTOS
               ELSE
200524*           COMPUTE WS-IMPORTE-REPARTOS = ((WS-IMPORTE-PERIOD -
200524*                                         WS-IMPORTE-206    -
200524*                                         WS-IMP-GASTOS) *
200524*                                         X350JPX5-PRIMA-ANUAL) /
200524*                                         X350JPX5-PRIMA-ANUAL
200524         COMPUTE WS-IMPORTE-REPARTOS = ((WS-IMPORTE-PERIOD-LTA -
200524*MF001                                    WS-IMPORTE-206    -
MF001                                           TB-ACUMULADO-206 (5) -
200524                                          WS-IMP-GASTOS-LTA) *
200524                                          X350JPX5-PRIMA-ANUAL) /
200524                                          X350JPX5-PRIMA-ANUAL

               END-IF

      *D*
      *        DISPLAY 'WS-IMPORTE-REPARTOS  '  WS-IMPORTE-REPARTOS
      *        DISPLAY 'WS-IMPORTE-PERIOD    '  WS-IMPORTE-PERIOD
      *        DISPLAY 'WS-IMPORTE-206       '  WS-IMPORTE-206
      *        DISPLAY 'X350JPX5-PRIMA-ANUAL '  X350JPX5-PRIMA-ANUAL
      *F*
                   MOVE WS-IMPORTE-REPARTOS TO X350JPX6-INGRESOS-GASTOS

                   IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
                      MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
                   END-IF

                   COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
                                                = WS-IMPORTE-REPARTOS
      *D*
      *        DISPLAY 'RSAL-INGRESOS-GASTOS-JOSE ' RSAL-INGRESOS-GASTOS
      *F*


                COMPUTE WS-NETA-COASEGUR ROUNDED
                                         = (WS-IMPORTE-REPARTOS *
                                            X350JPX3-PCOASE) / 100
                MOVE WS-NETA-COASEGUR         TO X350JPX6-NETA-COASEGUR

                IF X350JPX6-NETA-COASEGUR NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-NETA-COASEGUR
                END-IF

                COMPUTE RSAL-NETA-COASEGUR ROUNDED
                                           = WS-NETA-COASEGUR

                MOVE RSAL-P-PB-APLICAR  TO AUX-PB-TECNICA

110624          COMPUTE RSAL-PB-TECNICA ROUNDED
110624                                  = (WS-IMPORTE-REPARTOS *
110624                                     AUX-PB-TECNICA) / 100
                COMPUTE X350JPX6-PB-TECNICA = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100

                IF X350JPX6-PB-TECNICA NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-PB-TECNICA
                END-IF

                MOVE X350JPX3-PCOASE          TO AUX-PCOASE

                COMPUTE X350JPX6-DEV-NETCOASE = (WS-NETA-COASEGUR *
                                       AUX-PB-TECNICA) / 100

                IF X350JPX6-DEV-NETCOASE < ZEROES
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                   MOVE ZEROES TO RSAL-DEV-NETCOASE
                ELSE
                   COMPUTE RSAL-DEV-NETCOASE ROUNDED
                                          = (WS-NETA-COASEGUR *
                                          AUX-PB-TECNICA) / 100
                END-IF

                IF X350JPX6-DEV-NETCOASE NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                END-IF

200524*
200524*
200524*       COMPUTE WS-IMPORTE-REPARTOS = ((X350JPX5-PRIMA-PERIOD -
200524*                                       WS-IMPORTE-206    -
GTOMAR*                                       AUX-GASTOS-LTA) *
GTOMAR*                                       X350JPX5-GASTOS) *
200524*                                       X350JPX5-PRIMA-ANUAL) /
200524*                                       X350JPX5-PRIMA-ANUAL
200524*D*
200524*        DISPLAY 'COMPUTE-2            '  X350JPX5-PRIMA-PERIOD
200524*        DISPLAY 'X350JPX5-PRIMA-PERIOD ' X350JPX5-PRIMA-PERIOD
200524*        DISPLAY 'WS-IMPORTE-206       '  WS-IMPORTE-206
200524*        DISPLAY 'X350JPX5-GASTOS      '  X350JPX5-GASTOS
200524*        DISPLAY 'X350JPX5-PRIMA-ANUAL '  X350JPX5-PRIMA-ANUAL
200524*F*
200524*
200524*       MOVE WS-IMPORTE-REPARTOS TO X350JPX6-INGRESOS-GASTOS
200524*
200524*       IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
200524*          MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
200524*       END-IF
200524*
200524*           COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
200524*                                        = WS-IMPORTE-REPARTOS
200524*
200524*
200524*         COMPUTE WS-NETA-COASEGUR ROUNDED
200524*                                  = (WS-IMPORTE-REPARTOS *
200524*                                     X350JPX3-PCOASE) / 100
200524*         MOVE WS-NETA-COASEGUR         TO X350JPX6-NETA-COASEGUR
200524*
200524*         IF X350JPX6-NETA-COASEGUR NOT NUMERIC
200524*            MOVE ZEROES TO X350JPX6-NETA-COASEGUR
200524*         END-IF
200524*
200524*         COMPUTE RSAL-NETA-COASEGUR ROUNDED
200524*                                    = WS-NETA-COASEGUR
200524*
200524*         MOVE RSAL-P-PB-APLICAR  TO AUX-PB-TECNICA
200524*
200524*         COMPUTE RSAL-PB-TECNICA ROUNDED
200524*                                 = (WS-IMPORTE-REPARTOS *
200524*                                    AUX-PB-TECNICA) / 100
200524*         COMPUTE X350JPX6-PB-TECNICA = (WS-IMPORTE-REPARTOS *
200524*                                    AUX-PB-TECNICA) / 100
200524*
200524*         IF X350JPX6-PB-TECNICA NOT NUMERIC
200524*            MOVE ZEROES TO X350JPX6-PB-TECNICA
200524*         END-IF
200524*
200524*         MOVE X350JPX3-PCOASE          TO AUX-PCOASE
200524*
200524*         COMPUTE X350JPX6-DEV-NETCOASE = (WS-NETA-COASEGUR *
200524*                                AUX-PB-TECNICA) / 100
200524*
200524*         IF X350JPX6-DEV-NETCOASE < ZEROES
200524*            MOVE ZEROES TO X350JPX6-DEV-NETCOASE
200524*            MOVE ZEROES TO RSAL-DEV-NETCOASE
200524*         ELSE
200524*            COMPUTE RSAL-DEV-NETCOASE ROUNDED
200524*                                   = (WS-NETA-COASEGUR *
200524*                                   AUX-PB-TECNICA) / 100
200524*         END-IF
200524*
200524*         IF X350JPX6-DEV-NETCOASE NOT NUMERIC
200524*            MOVE ZEROES TO X350JPX6-DEV-NETCOASE
200524*         END-IF

PBTEC      END-IF

           IF RSAL-PGLOBAL = '0'
              MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA-REF1
              MOVE '/'                      TO RSAL-SEPARADOR
              MOVE '0000'                   TO RSAL-SUBPOL-REF1
           END-IF

           IF RSAL-POLIZA-REF1 = '0000000'
              MOVE SPACES TO RSAL-POLIZA-REF1
              MOVE SPACES TO RSAL-SEPARADOR
              MOVE SPACES TO RSAL-SUBPOL-REF1
           END-IF

           PERFORM 2200-RECUPERAR-UOA
           PERFORM 2300-OBTENER-NEW-GAP

           IF RSAL-REAJUSTE = 'S' AND  RSAL-REAJUSTE-PB = SPACES
              MOVE 'POL CON REAJUSTE CON SU TIPO PENDIENTE'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'B350JA12'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE '01'                TO X350J000-ID-ERROR-MOD
              PERFORM 2400-ESCRIBIR-ERRORES
           ELSE
              IF RSAL-REAJUSTE = 'S' AND
                 RSAL-REAJUSTE-PB NOT = SPACES
                 MOVE 'POL CON REAJUSTE CON SU TIPO INCONGRUENTE'
                                           TO X350J000-DESC-ERROR
                  MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                  MOVE 'B350JA12'          TO X350J000-ID-MODULO
                  MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                  MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                  MOVE 'W'                 TO X350J000-NIVEL
                  MOVE '02'                TO X350J000-ID-ERROR-MOD
                  PERFORM 2400-ESCRIBIR-ERRORES
                END-IF
           END-IF

           IF RSAL-REAJUSTE = 'N'           AND
              RSAL-REAJUSTE-PB NOT = SPACES OR
              AUX-PCALCULOPB-S NOT = ZEROES OR
              AUX-PPORCEN-REAL NOT = ZEROES
              MOVE 'POL SIN REAJUSTE PERO CON DATOS DEL MISMO'
                                       TO X350J000-DESC-ERROR
              MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
              MOVE 'B350JA12'          TO X350J000-ID-MODULO
              MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
              MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
              MOVE 'W'                 TO X350J000-NIVEL
              MOVE '03'                TO X350J000-ID-ERROR-MOD

              PERFORM 2400-ESCRIBIR-ERRORES
           END-IF

      *    PERFORM 5214-CALCULOS-LTB
           MOVE WS-IMPORTE-PBP         TO RSAL-PBPAGA

      *D*
      *          DISPLAY 'RSAL-FECHA CIERRE: ' WS-FECSIN-CIERRE
      *          DISPLAY 'RSAL-FECHA HASTA: ' WS-FECSIN-HASTA
      *F*
LUNA       MOVE RSAL-FCIERREA TO WS-FECSIN-ACIERRE
LUNA       MOVE RSAL-FCIERREM TO WS-FECSIN-MCIERRE
LUNA       MOVE RSAL-FCIERRED TO WS-FECSIN-DCIERRE
LUNA
1412       MOVE WS-FRENO-DESDE-ABONO-ACTUAL TO WS-FECSIN-ADESDE
1412       MOVE WS-FRENO-DESDE-MBONO-ACTUAL TO WS-FECSIN-MDESDE
1412       MOVE WS-FRENO-DESDE-DBONO-ACTUAL TO WS-FECSIN-DDESDE

LUNA       MOVE WS-FECSIN-DESDE TO RSAL-FECSIN-DESDE
LUNA       IF WS-FECSIN-CIERRE < WS-FECSIN-HASTA
LUNA          MOVE WS-FECSIN-CIERRE TO RSAL-FECSIN-HASTA
LUNA       ELSE
LUNA          MOVE WS-FECSIN-HASTA TO RSAL-FECSIN-HASTA
LUNA       END-IF

           WRITE REG-SALIDA1 FROM RSALIDA

           MOVE ZEROES TO WS-IMPORTE-PERIOD-LTA

      *    MOVE WS-POLIZA-2 TO WS-POLIZA-2-ANT
      *    MOVE WS-SUBPOL-2 TO WS-SUBPOL-2-ANT
      *    MOVE WS-POLIZA-4 TO WS-POLIZA-4-ANT
      *    MOVE WS-SUBPOL-4 TO WS-SUBPOL-4-ANT

           INITIALIZE RSALIDA
LTA        .
      ******************************************************************10650099
       5100-GRABAR-SALIDA-TOT.                                          10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5100-GRABAR-SALIDA-TOT'
      *    DISPLAY 'X350JPX6-CANAL ' X350JPX6-CANAL
      *    DISPLAY 'X350JPX6-MODALIDAD ' X350JPX6-MODALIDAD
      *    DISPLAY 'X350JPX6-RENOVADA ' X350JPX6-RENOVADA
      *    DISPLAY 'X350JPX6-GAP ' X350JPX6-GAP
      *    DISPLAY 'X350JPX6-UOACOUNT ' X350JPX6-UOACOUNT
      *    DISPLAY 'X350JPX6-FCIERREA ' X350JPX6-FCIERREA
      *    DISPLAY 'X350JPX6-PRIMA-ANUAL ' X350JPX6-PRIMA-ANUAL
      *    DISPLAY 'X350JPX6-PRIMA-RECIBOS ' X350JPX6-PRIMA-RECIBOS
      *    DISPLAY 'X350JPX6-APUNTE-RECIBO ' X350JPX6-APUNTE-RECIBO
      *    DISPLAY 'X350JPX6-PRIMA-PERIOD ' X350JPX6-PRIMA-PERIOD
      *    DISPLAY 'X350JPX6-GASTOS ' X350JPX6-GASTOS
      *    DISPLAY 'X350JPX6-TOTAL-SINIES ' X350JPX6-TOTAL-SINIES
      *    DISPLAY 'X350JPX6-INGRESOS-GASTOS '
      *                                     X350JPX6-INGRESOS-GASTOS
      *    DISPLAY 'X350JPX6-NETA-COASEGUR ' X350JPX6-NETA-COASEGUR
      *    DISPLAY 'X350JPX6-PB-TECNICA ' X350JPX6-PB-TECNICA
      *    DISPLAY 'X350JPX6-DEV-NETCOASE ' X350JPX6-DEV-NETCOASE
      *F*
           INITIALIZE REG-SALIDA3

           PERFORM 2200-RECUPERAR-UOA
           PERFORM 2300-OBTENER-NEW-GAP-MOV

           MOVE ZEROES   TO X350JPX6-PRIMA-ANUAL
           MOVE ZEROES   TO X350JPX6-INGRESOS-GASTOS
           MOVE ZEROES   TO X350JPX6-NETA-COASEGUR
           MOVE ZEROES   TO X350JPX6-PB-TECNICA
           MOVE ZEROES   TO X350JPX6-DEV-NETCOASE
           MOVE RSAL-GAP TO X350JPX6-GAP

           WRITE REG-SALIDA3 FROM X350JPX6

           INITIALIZE X350JPX6
           .
      ******************************************************************10650099
       5100-GRABAR-SALIDA-SUSCRIP.                                      10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5100-GRABAR-SALIDA-SUSCRIP'
      *F*
           MOVE RSAL-GAP TO X350JPX6-GAP

LUNA       MOVE RSAL-FCIERREA TO WS-FECSIN-ACIERRE
LUNA       MOVE RSAL-FCIERREM TO WS-FECSIN-MCIERRE
LUNA       MOVE RSAL-FCIERRED TO WS-FECSIN-DCIERRE
LUNA
LUNA       MOVE WS-FECSIN-DESDE TO RSAL-FECSIN-DESDE
LUNA       IF WS-FECSIN-CIERRE < WS-FECSIN-HASTA
LUNA          MOVE WS-FECSIN-CIERRE TO RSAL-FECSIN-HASTA
LUNA       ELSE
LUNA          MOVE WS-FECSIN-HASTA TO RSAL-FECSIN-HASTA
LUNA       END-IF

RF001         IF WS-RENOVADA EQUAL "N" AND SW-SI-LTA-SOLO-ANT-P
RF001            CONTINUE
RF001         ELSE
                 WRITE REG-SALIDA1 FROM RSALIDA
                 WRITE REG-SALIDA3 FROM X350JPX6

RF001         END-IF
           MOVE WS-POLIZA-2 TO WS-POLIZA-2-ANT
           MOVE WS-SUBPOL-2 TO WS-SUBPOL-2-ANT

           IF RSAL-RENOVADA = 'S'
              INITIALIZE RSALIDA
                         X350JPX6
           ELSE
              PERFORM 5200-GRABAR-SALIDA-SUSCRIP-NR
           END-IF
           .
      *
      ******************************************************************10650099
0124   5100-GRABAR-SALIDA-SI-FINAL.                                     10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5100-GRABAR-SALIDA-SUSCRIP-SI-FINAL'
      *    DISPLAY 'WS-POLIZA-4 ' WS-POLIZA-4
      *    DISPLAY 'WS-SUBPOL-4 ' WS-SUBPOL-4
      *    DISPLAY 'WS-POLIZA-4-ANT ' WS-POLIZA-4-ANT
      *    DISPLAY 'WS-SUBPOL-4-ANT ' WS-SUBPOL-4-ANT
      *F*
           WRITE REG-SALIDA1 FROM RSALIDA
0124       INITIALIZE RSALIDA
           .
      *
0124  *
      ******************************************************************10650099
       5200-GRABAR-SALIDA-SUSCRIP-NR.                                   10660099
      ******************************************************************10670099
SINUSO*MF001 INITIALIZE DATOS-CTEC
MF001      INITIALIZE
                      RSALIDA
      *               X350JPX6

                MOVE ZEROES TO X350JPX6-PRIMA-ANUAL
                               X350JPX6-INGRESOS-GASTOS
                               X350JPX6-NETA-COASEGUR
                               X350JPX6-PB-TECNICA
                               X350JPX6-DEV-NETCOASE

      *D*
      *         DISPLAY '5200-GRABAR-SALIDA-SUSCRIP-NR'
      *         DISPLAY 'X350JPX3-FRENO-DESDE ' X350JPX3-FRENO-DESDE
      *         DISPLAY 'X350JPX3-FRENO-HASTA ' X350JPX3-FRENO-HASTA
      *         DISPLAY 'FECHA DESDE PB 2' WS-FECSIN-DESDE-PB
      *         DISPLAY 'FECHA HASTA ' WS-FECSIN-HASTA
      *F*
                MOVE '3000'                   TO RSAL-MODELO

                MOVE X350JPX3-FRENO-DESDE TO WS-FECHA-DESDE-NR
                MOVE X350JPX3-FRENO-HASTA TO WS-FECHA-HASTA-NR

                MOVE X350JPX3-FRENO-DESDE TO WS-FECHA-AUX-CNTL

                COMPUTE WS-ANIO-CNTL    = WS-ANIO-CNTL + 1

                IF WS-FECHA-AUX-CNTL < X350JPX3-FRENO-HASTA
                   MOVE WS-FECHA-AUX-CNTL TO X350JPX3-FRENO-HASTA

                END-IF

PBTEC           IF X350JPX3-FRENO-HASTA   >  R350T000-FFINAL
PBTEC *         OR X350JPX3-FRENO-HASTA   =  R350T000-FFINAL
                   MOVE 'S'                   TO RSAL-RENOVADA
                                                 X350JPX6-RENOVADA

                ELSE
                   MOVE 'N'                   TO RSAL-RENOVADA
                                                 X350JPX6-RENOVADA

                END-IF
      *

                MOVE X350JPX3-CCANAL          TO RSAL-CANAL
                                                 X350JPX6-CANAL
                MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA
                MOVE X350JPX3-KSUBPOL         TO RSAL-SUBPOLIZA
                MOVE SPACES                   TO RSAL-SUSCRIP
      *
                MOVE X350JPX3-KRAMO           TO RSAL-RAMO
                                                 X350JPX6-RAMO
                MOVE X350JPX3-MODALI          TO RSAL-MODALIDAD
                                                 X350JPX6-MODALIDAD
                MOVE X350JPX3-VIGOR-ANUL      TO RSAL-CESTADOASEG
                MOVE '00'                     TO RSAL-PREGRUPO(1:2)
                MOVE X350JPX3-CARTINV         TO RSAL-PREGRUPO(3:2)

PBTECJ          MOVE WS-ANNO-HASNR            TO RSAL-FRENO-ADESDE
                MOVE WS-MES-HASNR             TO RSAL-FRENO-MDESDE
                MOVE WS-DIA-HASNR             TO RSAL-FRENO-DDESDE

                MOVE WS-FECHA-HASTA-NR        TO WS-FECHA-NR


      *         MOVE X350JPX3-FRENO-AHASTA    TO RSAL-FRENO-ADESDE
      *         MOVE X350JPX3-FRENO-MHASTA    TO RSAL-FRENO-MDESDE
      *         MOVE X350JPX3-FRENO-DHASTA    TO RSAL-FRENO-DDESDE
      *
      *         MOVE X350JPX3-FRENO-HASTA     TO WS-FECHA-NR
                COMPUTE WS-ANNO-NR = WS-ANNO-NR + 1

                MOVE WS-ANNO-NR               TO RSAL-FRENO-AHASTA
                MOVE WS-MES-HASNR             TO RSAL-FRENO-MHASTA
                MOVE WS-DIA-HASNR             TO RSAL-FRENO-DHASTA

      *         IF X350JPX3-KPOLIZA = '0484811' AND
      *            X350JPX3-KSUBPOL = '0000'
      *D*
      *       DISPLAY 'X350JPX3-FRENO-AHASTA' X350JPX3-FRENO-AHASTA
      *       DISPLAY 'X350JPX3-FRENO-MHASTA' X350JPX3-FRENO-MHASTA
      *       DISPLAY 'X350JPX3-FRENO-DHASTA' X350JPX3-FRENO-DHASTA
      *       DISPLAY 'WS-ANNO-NR           ' WS-ANNO-NR
      *       DISPLAY 'WS-MES-NR            ' WS-MES-NR
      *       DISPLAY 'WS-DIA-NR            ' WS-DIA-NR
      *F*
      *         END-IF

                MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
                                                 X350JPX6-FCIERREA
                MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
                                                 X350JPX6-FCIERREM
                MOVE R350T000-FDFINAL         TO RSAL-FCIERRED
                                                 X350JPX6-FCIERRED
LTA             MOVE 'PB'                     TO RSAL-LTA

                IF X350JPX3-VIGOR-ANUL  = 'A'
                   MOVE X350JPX3-ANO-ANUL     TO RSAL-FECAANULACION
                   MOVE X350JPX3-MES-ANUL     TO RSAL-FECMANULACION
                   MOVE X350JPX3-DIA-ANUL     TO RSAL-FECDANULACION
                   MOVE X350JPX3-MOTIVO-ANU   TO RSAL-MOTIVO
                END-IF

                MOVE X350JPX3-FCOBRO          TO RSAL-FORMAPAGO
                MOVE X350JPX3-TIPO-COASE      TO RSAL-COASEGURO
                MOVE X350JPX3-PCOASE          TO RSAL-PCOASEGURO
                MOVE X350JPX3-PAGADOR         TO RSAL-PGLOBAL

               IF X350JPX3-CERTI EQUAL '000000' AND                     37430000
                  X350JPX3-ASEG-MECANIZADOS = 'N'                       37440000
                  MOVE 'S' TO RSAL-IMNOMINADA                           37490000

               ELSE
                  MOVE 'N' TO RSAL-IMNOMINADA                           37490000

               END-IF

                MOVE X350JPX3-NASEGURADO      TO RSAL-NASEGURADOS


200524          IF SW-SI-LTB
200524             MOVE X350M004-KGRUPEMP-BS(3:3) TO X350JPX3-KGRUEMP
200524          END-IF

200524          IF X350JPX3-KGRUEMP = ZEROS
200524            MOVE SPACES           TO X350JPX3-KGRUEMP
200524          END-IF
                MOVE X350JPX3-KGRUEMP         TO RSAL-GEMPRESA

PBTECJ*    IF AUX-TIP-REAJUSTE = '1' OR 'T'
      *       MOVE 'S'                   TO RSAL-REAJUSTE
      *    ELSE
      *       MOVE 'N'                   TO RSAL-REAJUSTE
      *    END-IF

PBTECJ          MOVE AUX-CCOBON-REAJUS        TO RSAL-REAJUSTE

                MOVE AUX-REAJUSTE-PB          TO RSAL-REAJUSTE-PB
      *
                MOVE WS-REAJREC               TO RSAL-REAJUSTE-REC
                MOVE AUX-TIP-REAJUSTE         TO RSAL-TIP-REAJUSTE
                MOVE AUX-NANIOPER             TO RSAL-APERDIDAS
                MOVE AUX-NANIOBEN             TO RSAL-ABENEFICI
                MOVE AUX-PANTICIPO-S          TO RSAL-P-ANTICIPO
                MOVE AUX-CCUADROBAREMO-S      TO RSAL-BAREMO
                MOVE AUX-PPORCEN-REAL         TO RSAL-REAJUSTEO
                MOVE AUX-PCALCULOPB-S         TO RSAL-P-REAUSTE-PB

           IF X350X098-IANTICIPO-S > ZEROES
              MOVE 'S'                   TO RSAL-ANTICIPO-PB
           ELSE
              MOVE 'N'                   TO RSAL-ANTICIPO-PB
           END-IF

           IF AUX-PPORCEN-REAL    = ZEROES
              MOVE AUX-PCALCULOPB-S      TO RSAL-P-PB-APLICAR
              IF AUX-CCUADROBAREMO-S EQUAL ZEROES
              MOVE SPACES                TO RSAL-BAREMO
              END-IF
              IF AUX-PCALCULOPB-S = ZEROES
                 MOVE ZEROES TO RSAL-P-PB-APLICAR
                 IF AUX-CCUADROBAREMO-S EQUAL ZEROES
                 MOVE SPACES              TO RSAL-BAREMO
                 END-IF
              END-IF
           ELSE
              MOVE AUX-PPORCEN-REAL      TO RSAL-P-PB-APLICAR

           END-IF

                IF AUX-PPORCEN-REAL NOT = ZEROES AND
                   AUX-PCALCULOPB-S NOT = ZEROES
                   MOVE AUX-CCUADROBAREMO-S TO RSAL-BAREMO
PBTEC              MOVE AUX-PCALCULOPB-S    TO RSAL-P-PB-APLICAR
                END-IF

                MOVE AUX-PGASTOS-S            TO RSAL-P-GASTOS-PB

                COMPUTE RSAL-PRIMA-RECIBOS ROUNDED = WS-IMPORTE-PRI
230524*         COMPUTE RSAL-PRIMA-RECIBOS ROUNDED = WS-IMPORTE-PRI
230524          COMPUTE RSAL-PRIMA-RECIBOS ROUNDED = X350JPX3-EPNETA

230524*         MOVE WS-IMPORTE-PRI           TO X350JPX6-PRIMA-RECIBOS
230524          MOVE X350JPX3-EPNETA          TO X350JPX6-PRIMA-RECIBOS

                MOVE ZEROES                   TO RSAL-APUNTE-RECIBO
                                                 X350JPX6-APUNTE-RECIBO

                PERFORM CALCULAR-ANNOS-TRANS-MVT

                PERFORM 3300-RECUPERAR-ALFA-NR

240125*         IF WS-IMPORTE-902 = ZEROES
                   COMPUTE RSAL-PRIMA-PERIOD ROUNDED
230524*                    = (WS-IMPORTE-PRI) * WS-ALFA
230524                     = (X350JPX3-EPNETA) * WS-ALFA

230524*            COMPUTE WS-IMPORTE-PERIOD = (WS-IMPORTE-PRI)
230524             COMPUTE WS-IMPORTE-PERIOD = (X350JPX3-EPNETA)
                           * WS-ALFA

230524*            COMPUTE X350JPX6-PRIMA-PERIOD = (WS-IMPORTE-PRI)
230524             COMPUTE X350JPX6-PRIMA-PERIOD = (X350JPX3-EPNETA)
                           * WS-ALFA

LTA                COMPUTE WS-IMPORTE-PERIOD-LTA = (WS-IMPORTE-PERIOD +
LTA                        WS-IMPORTE-PERIOD-LTA)
      *D*
      *         DISPLAY ' WS-ALFA -NR-PB        '  WS-ALFA
      *         DISPLAY ' WS-IMPORTE-PERIOD-LTA '  WS-IMPORTE-PERIOD-LTA
      *F*
240125*         ELSE
240125*            MOVE WS-IMPORTE-902 TO RSAL-PRIMA-RECIBOS
240125*            MOVE WS-IMPORTE-902 TO X350JPX6-PRIMA-RECIBOS
240125*
240125*            COMPUTE RSAL-PRIMA-PERIOD ROUNDED
240125*                    = (WS-IMPORTE-902) * WS-ALFA
240125*
240125*            COMPUTE WS-IMPORTE-PERIOD = (WS-IMPORTE-902)
240125*                    * WS-ALFA
240125*
240125*            COMPUTE X350JPX6-PRIMA-PERIOD = (WS-IMPORTE-902)
240125*                    * WS-ALFA
240125*
LTA                COMPUTE WS-IMPORTE-PERIOD-LTA = (WS-IMPORTE-902)
LTA                        * WS-ALFA
      *D*
      *         DISPLAY ' WS-IMPORTE-PERIOD-LTA '  WS-IMPORTE-PERIOD-LTA
      *F*
240125*         END-IF

                COMPUTE WS-IMP-GASTOS = (WS-IMPORTE-PERIOD *
                                          AUX-PGASTOS-S) / 100

                MOVE WS-IMP-GASTOS            TO X350JPX6-GASTOS

                COMPUTE RSAL-GASTOS ROUNDED = WS-IMP-GASTOS

130924*****     IF X350JPX3-FRENO-AHASTA = R350T000-FAFINAL
161224******    IF X350JPX3-FRENO-AHASTA = WS-ANNO-HASTA-TRAMO
191224          IF X350JPX3-FRENO-AHASTA > WS-ANNO-HASTA-TRAMO
130924            COMPUTE WS-IMPORTE-205-BONO = WS-IMPORTE-205
130924          ELSE
JOSEN             COMPUTE WS-IMPORTE-205-BONO = (WS-IMPORTE-205    +
130924                                  WS-IMPORTE-PERIOD)
PLURIN             MOVE WS-IMPORTE-PERIOD TO WS-IMPORTE-PERIOD-BONO
PLURIN             MOVE WS-IMPORTE-205    TO WS-IMPORTE-PERIOD-BONO-ANT
                END-IF
      *D*
      *         DISPLAY ' X350JPX3-FRENO-AHASTA '  X350JPX3-FRENO-AHASTA
      *         DISPLAY ' R350T000-FAFINAL      '  R350T000-FAFINAL
      *         DISPLAY ' WS-IMPORTE-205-BONO   '  WS-IMPORTE-205-BONO
      *         DISPLAY ' WS-IMPORTE-205        '  WS-IMPORTE-205
      *         DISPLAY ' WS-IMPORTE-PERIOD     '  WS-IMPORTE-PERIOD
      *F*
MF001 *         MOVE WS-IMPORTE-206-NR        TO X350JPX6-TOTAL-SINIES
MF001           MOVE TB-ACUMULADO-206 (1)     TO X350JPX6-TOTAL-SINIES

MF001 *         COMPUTE RSAL-TOTAL-SINIES ROUNDED = WS-IMPORTE-206-NR
MF001           COMPUTE RSAL-TOTAL-SINIES ROUNDED = TB-ACUMULADO-206 (1)

                MOVE X350X098-IARRASTRE-S     TO RSAL-IMP-ARRAS-PERD

                IF X350JPX3-KGRUEMP = '000'
                  MOVE SPACES           TO X350JPX3-KGRUEMP
                END-IF

                IF X350JPX3-KGRUEMP NOT = SPACES
                   OPEN INPUT ENTRADA-TASE
                   PERFORM 2703-LEER-TOTALES-ASEGURADO
                     UNTIL X350JPX3-KGRUEMP EQUAL
                           X350JPX4-KGRUEMP
                  MOVE X350JPX4-NASEGURADO    TO RSAL-ASE-TOT-EMPRESA
                  COMPUTE RSAL-PRI-TOT-EMPRESA ROUNDED
                                              = X350JPX4-PERIOD
PBTECJ*                                       = X350JPX4-PERIOD-NR

                  COMPUTE RSAL-SIN-TOT-EMPRESA ROUNDED
                                              = X350JPX4-SINIES
PBTECJ*                                       = X350JPX4-SINIES-NR

                  COMPUTE RSAL-GASTOS-TOT-EMPR ROUNDED
                                              = X350JPX4-GASTOS
PBTECJ*                                       = X350JPX4-GASTOS-NR

      *D*
PBTECJ*       DISPLAY 'NO RENO          '
      *       DISPLAY 'X350JPX6-FCIERREA' X350JPX6-FCIERREA
      *       DISPLAY 'X350JPX6-FCIERREM' X350JPX6-FCIERREM
      *       DISPLAY 'X350JPX6-FCIERRED' X350JPX6-FCIERRED
      *       DISPLAY 'R350T000-FAFINAL ' R350T000-FAFINAL
      *       DISPLAY 'R350T000-FMFINAL ' R350T000-FMFINAL
      *       DISPLAY 'R350T000-FDFINAL ' R350T000-FDFINAL
      *       DISPLAY 'X350JPX4-PERIOD  ' X350JPX4-PERIOD
      *       DISPLAY 'X350JPX4-SINIES  ' X350JPX4-SINIES
      *       DISPLAY 'X350JPX4-GASTOS  ' X350JPX4-GASTOS
      *       DISPLAY 'R350T000-FFINAL  ' R350T000-FFINAL
      *       DISPLAY 'WS-FECHA-HASTA-NR      ' WS-FECHA-HASTA-NR
      *F*
                  IF X350JPX6-FCIERREA = R350T000-FAFINAL AND
                     X350JPX6-FCIERREM = R350T000-FMFINAL AND
                     X350JPX6-FCIERRED = R350T000-FDFINAL

                     INITIALIZE RSAL-PRI-TOT-EMPRESA
                                RSAL-SIN-TOT-EMPRESA
                                RSAL-GASTOS-TOT-EMPR

      *           MOVE X350JXX2-FRENO-HASTA TO WS-FECHA-PRIMA-EMPRESA

PBTECJ                  IF WS-FECHA-HASTA-NR NOT EQUAL
                           R350T000-FFINAL
                           COMPUTE RSAL-PRI-TOT-EMPRESA ROUNDED
                                                 = X350JPX4-PERIOD
                           COMPUTE RSAL-SIN-TOT-EMPRESA ROUNDED
                                                 = X350JPX4-SINIES
                           COMPUTE RSAL-GASTOS-TOT-EMPR ROUNDED
                                                 = X350JPX4-GASTOS
                        ELSE

                     COMPUTE RSAL-PRI-TOT-EMPRESA ROUNDED
                                              = X350JPX4-PERIOD-NR

                     COMPUTE RSAL-SIN-TOT-EMPRESA ROUNDED
                                              = X350JPX4-SINIES-NR

                     COMPUTE RSAL-GASTOS-TOT-EMPR ROUNDED
                                              = X350JPX4-GASTOS-NR
                        END-IF
                  END-IF

                  CLOSE ENTRADA-TASE
                END-IF

                IF RSAL-PGLOBAL = '0'
                   MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA-REF1
                   MOVE '/'                      TO RSAL-SEPARADOR
                   MOVE '0000'                   TO RSAL-SUBPOL-REF1
                END-IF

                IF RSAL-POLIZA-REF1 = '0000000'
                   MOVE SPACES TO RSAL-POLIZA-REF1
                   MOVE SPACES TO RSAL-SEPARADOR
                   MOVE SPACES TO RSAL-SUBPOL-REF1
                END-IF

      *         MOVE X350JPX3-FRENO-HASTA    TO X350JXX2-FRENO-DESDE
      *         MOVE WS-FECHA-NR             TO X350JXX2-FRENO-HASTA
                MOVE X350JPX3-CTIPOALFA      TO X350JXX2-CTIPOALFA
                MOVE X350JPX3-FEFECTO-INIPOL TO X350JXX2-FEFECTO-INIPOL

                MOVE WS-ALFA                  TO RSAL-ALFA

                PERFORM 2300-OBTENER-NEW-GAP

                MOVE RSAL-GAP                 TO X350JPX6-GAP
                MOVE SPACES                   TO RSAL-GAP

      *D*
      *         DISPLAY 'RSAL-FECHA CIERRE: ' WS-FECSIN-CIERRE
      *         DISPLAY 'RSAL-FECHA pb desde: ' WS-FECSIN-DESDE-PB
      *         DISPLAY 'WS-RENOVAD: ' WS-RENOVADA
      *         DISPLAY 'RWS-ANTPBAN: ' WS-ANTPBAN
      *F*
LUNA       MOVE RSAL-FCIERREA TO WS-FECSIN-ACIERRE
LUNA       MOVE RSAL-FCIERREM TO WS-FECSIN-MCIERRE
LUNA       MOVE RSAL-FCIERRED TO WS-FECSIN-DCIERRE


LUNN       IF WS-RENOVADA = 'N' AND WS-ANTPBAN = 'S' AND RSAL-LTA = 'PB'

JOSEN *           IF WS-FECSIN-DESDE > WS-FECSIN-HASTA
JOSEN *              MOVE WS-FECSIN-HASTA TO WS-FECSIN-DESDE-PB
                  IF WS-FECSIN-HASTA  > WS-FECSIN-DESDE-PB
                     CONTINUE
                  ELSE
JOSEN                MOVE WS-FECSIN-HASTA TO WS-FECSIN-DESDE-PB
                  END-IF

      *D*
      *         DISPLAY 'ENTRO IF              '
      *F*
JOSEN *           ELSE
JOSEN *              MOVE WS-FECSIN-HASTA TO WS-FECSIN-DESDE-PB
LUNN                 MOVE WS-FECSIN-DESDE-PB TO WS-FECSIN-DESDE
JOSEN                ADD 1 TO WS-FECSIN-AHASTA
      *           END-IF
LUNN       END-IF

LUNA
LUNA       MOVE WS-FECSIN-DESDE TO RSAL-FECSIN-DESDE
LUNA       IF WS-FECSIN-CIERRE < WS-FECSIN-HASTA
LUNA          MOVE WS-FECSIN-CIERRE TO RSAL-FECSIN-HASTA
LUNA       ELSE
LUNA          MOVE WS-FECSIN-HASTA TO RSAL-FECSIN-HASTA
LUNA       END-IF




           WRITE REG-SALIDA1 FROM RSALIDA
           WRITE REG-SALIDA3 FROM X350JPX6

SINUSO*MF001 INITIALIZE DATOS-CTEC
MF001      INITIALIZE
                      RSAL-PRIMA-RECIBOS
                      X350JPX6-PRIMA-RECIBOS
                      RSAL-APUNTE-RECIBO
                      X350JPX6-APUNTE-RECIBO
                      RSAL-TOTAL-SINIES
                      X350JPX6-TOTAL-SINIES
                      RSAL-PRIMA-PERIOD
                      X350JPX6-PRIMA-PERIOD
                      RSAL-PRI-TOT-EMPRESA
                      RSAL-ASE-TOT-EMPRESA
                      RSAL-GASTOS-TOT-EMPR
                      RSAL-SIN-TOT-EMPRESA
                      RSAL-INGRESOS-GASTOS
                      X350JPX6-INGRESOS-GASTOS
                      RSAL-NETA-COASEGUR
                      X350JPX6-NETA-COASEGUR
                      RSAL-PB-TECNICA
                      X350JPX6-PB-TECNICA
                      RSAL-DEV-NETCOASE
                      X350JPX6-DEV-NETCOASE

      *D*
      *         DISPLAY '5200-GRABAR-SALIDA-SUSCRIP-NR-1'
      *F*
           MOVE '3000'                   TO RSAL-MODELO
           MOVE X350JPX3-CCANAL          TO RSAL-CANAL
                                            X350JPX6-CANAL
           MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA
           MOVE X350JPX3-KSUBPOL         TO RSAL-SUBPOLIZA
           MOVE X350JPX3-KSUSCRI         TO RSAL-SUSCRIP

           MOVE X350JPX3-FEFECTO-AINIPOL TO RSAL-FORIGENASUSCRI
           MOVE X350JPX3-FEFECTO-MINIPOL TO RSAL-FORIGENMSUSCRI
           MOVE X350JPX3-FEFECTO-DINIPOL TO RSAL-FORIGENDSUSCRI

           MOVE X350JPX3-KRAMO           TO RSAL-RAMO
                                            X350JPX6-RAMO
           MOVE X350JPX3-MODALI          TO RSAL-MODALIDAD
                                            X350JPX6-MODALIDAD
           MOVE X350JPX3-VIGOR-ANUL      TO RSAL-CESTADOASEG
           MOVE '00'                     TO RSAL-PREGRUPO(1:2)
           MOVE X350JPX3-CARTINV         TO RSAL-PREGRUPO(3:2)
           MOVE X350JPX3-CASADA          TO RSAL-CASADO
           MOVE X350JPX3-PINTER-TECBASE  TO RSAL-I1BTI

           MOVE X350JPX3-FRENO-AHASTA    TO RSAL-FRENO-ADESDE
           MOVE WS-MES-HASNR             TO RSAL-FRENO-MDESDE
           MOVE WS-DIA-HASNR             TO RSAL-FRENO-DDESDE

           MOVE WS-ANNO-NR            TO RSAL-FRENO-AHASTA
           MOVE WS-MES-HASNR          TO RSAL-FRENO-MHASTA
           MOVE WS-DIA-HASNR          TO RSAL-FRENO-DHASTA

           MOVE R350T000-FAFINAL         TO RSAL-FCIERREA
                                            X350JPX6-FCIERREA
           MOVE R350T000-FMFINAL         TO RSAL-FCIERREM
                                            X350JPX6-FCIERREM
           MOVE R350T000-FDFINAL         TO RSAL-FCIERRED
                                            X350JPX6-FCIERRED
           MOVE 'PB'                     TO RSAL-LTA
           MOVE X350JPX3-FCOBRO          TO RSAL-FORMAPAGO
           MOVE X350JPX3-TIPO-COASE      TO RSAL-COASEGURO
           MOVE X350JPX3-PCOASE          TO RSAL-PCOASEGURO
           MOVE X350JPX3-PAGADOR         TO RSAL-PGLOBAL

           IF X350JPX3-CERTI EQUAL '000000' AND                         000
              X350JPX3-ASEG-MECANIZADOS = 'N'                           000
              MOVE 'S' TO RSAL-IMNOMINADA                               000
           ELSE
              MOVE 'N' TO RSAL-IMNOMINADA                               000
           END-IF

           MOVE X350JPX3-NASEGURADO      TO RSAL-NASEGURADOS

           IF X350JPX3-VIGOR-ANUL  = 'A'
              MOVE X350JPX3-ANO-ANUL     TO RSAL-FECAANULACION
              MOVE X350JPX3-MES-ANUL     TO RSAL-FECMANULACION
              MOVE X350JPX3-DIA-ANUL     TO RSAL-FECDANULACION
              MOVE X350JPX3-MOTIVO-ANU   TO RSAL-MOTIVO
           END-IF

           MOVE WS-ALFA                  TO RSAL-ALFA

           MOVE X350JPX3-KGRUEMP         TO RSAL-GEMPRESA

PBTECJ*    IF AUX-TIP-REAJUSTE = '1' OR 'T'
      *       MOVE 'S'                   TO RSAL-REAJUSTE
      *    ELSE
      *       MOVE 'N'                   TO RSAL-REAJUSTE
      *    END-IF

PBTECJ     MOVE AUX-CCOBON-REAJUS        TO RSAL-REAJUSTE

           MOVE AUX-REAJUSTE-PB          TO RSAL-REAJUSTE-PB

           MOVE WS-REAJREC               TO RSAL-REAJUSTE-REC
           MOVE AUX-TIP-REAJUSTE         TO RSAL-TIP-REAJUSTE
           MOVE AUX-NANIOPER             TO RSAL-APERDIDAS
           MOVE AUX-NANIOBEN             TO RSAL-ABENEFICI

           IF X350X098-IANTICIPO-S > ZEROES
              MOVE 'S'                   TO RSAL-ANTICIPO-PB
           ELSE
              MOVE 'N'                   TO RSAL-ANTICIPO-PB
           END-IF
           MOVE AUX-PANTICIPO-S          TO RSAL-P-ANTICIPO
           MOVE AUX-CCUADROBAREMO-S      TO RSAL-BAREMO
           MOVE AUX-PCALCULOPB-S         TO RSAL-P-PB-APLICAR
           MOVE AUX-PPORCEN-REAL         TO RSAL-REAJUSTEO
           MOVE AUX-PCALCULOPB-S         TO RSAL-P-REAUSTE-PB

           IF AUX-PPORCEN-REAL    = ZEROES
              MOVE AUX-PCALCULOPB-S      TO RSAL-P-PB-APLICAR
              IF AUX-CCUADROBAREMO-S EQUAL ZEROES
              MOVE SPACES                TO RSAL-BAREMO
              END-IF
              IF AUX-PCALCULOPB-S = ZEROES
                 MOVE ZEROES TO RSAL-P-PB-APLICAR
                 IF AUX-CCUADROBAREMO-S EQUAL ZEROES
                 MOVE SPACES              TO RSAL-BAREMO
                 END-IF
              END-IF
           ELSE
              MOVE AUX-PPORCEN-REAL      TO RSAL-P-PB-APLICAR

           END-IF

           IF AUX-PPORCEN-REAL NOT = ZEROES AND
              AUX-PCALCULOPB-S NOT = ZEROES
              MOVE AUX-CCUADROBAREMO-S TO RSAL-BAREMO
PBTEC         MOVE AUX-PCALCULOPB-S    TO RSAL-P-PB-APLICAR
           END-IF

           MOVE AUX-PGASTOS-S            TO RSAL-P-GASTOS-PB
PBTEC      MOVE WS-REAJREC               TO RSAL-REAJUSTE-REC

      *    COMPUTE RSAL-PRIMA-ANUAL ROUNDED = WS-IMPORTE-PRI
josen *    COMPUTE RSAL-PRIMA-ANUAL ROUNDED = X350JXX2-EPNETA
josen      COMPUTE RSAL-PRIMA-ANUAL ROUNDED = X350JPX3-EPNETA

           MOVE X350JPX3-EPNETA          TO X350JPX6-PRIMA-ANUAL
           MOVE ZEROES                   TO RSAL-GASTOS
                                            X350JPX6-GASTOS
           MOVE X350X098-IARRASTRE-S     TO RSAL-IMP-ARRAS-PERD
           MOVE ZEROES                   TO RSAL-NETA-COASEGUR
                                            X350JPX6-NETA-COASEGUR

           IF X350JPX3-KGRUEMP = '000'
              MOVE SPACES           TO X350JPX3-KGRUEMP
           END-IF

           IF X350JPX3-KGRUEMP NOT = SPACES
             MOVE X350JPX4-NASEGURADO    TO RSAL-ASE-TOT-EMPRESA
           END-IF

           IF X350JPX3-KGRUEMP NOT = SPACES
      *D*
      *       DISPLAY 'X350JPX3-KGRUEMP ' X350JPX3-KGRUEMP
      *F*
              OPEN INPUT ENTRADA-TASE
              PERFORM 2703-LEER-TOTALES-ASEGURADO
                UNTIL X350JPX3-KGRUEMP EQUAL
                      X350JPX4-KGRUEMP
                CLOSE ENTRADA-TASE

                OPEN INPUT ENTRADA-TASE-CP
                PERFORM 2703-LEER-TOTALES-ASEGURADO-CP
                  UNTIL X350JPX3-KPOLIZA EQUAL
                        X350JPX5-KPOLIZA AND
                        X350JPX3-KSUBPOL EQUAL
                        X350JPX5-KSUBPOL
                  CLOSE ENTRADA-TASE-CP

                  IF X350JPX5-PRIMA-ANUAL <= ZEROES
                     MOVE ZEROES TO WS-IMPORTE-REPARTOS
                  ELSE
      *D*
      *           DISPLAY 'PRUEBA REPARTOS'
      *           DISPLAY 'X350JPX4-PERIOD-NR ' X350JPX4-PERIOD-NR
      *           DISPLAY 'X350JPX4-GASTOS-NR ' X350JPX4-GASTOS-NR
      *           DISPLAY 'X350JPX4-SINIES-NR ' X350JPX4-SINIES-NR
      *           DISPLAY 'TOT PRI ' X350JPX5-PRIMA-ANUAL
      *F*
PBTECJ*              COMPUTE WS-IMPORTE-REPARTOS =
      *                               (((WS-IMPORTE-PRI) * WS-ALFA -
      *                                          WS-IMP-GASTOS     -
      *                                          WS-IMPORTE-206-NR   ) *
      *                                          WS-IMPORTE-PRI      ) /
      *                                          X350JPX4-PRIMA-ANUAL
PBTECJ*070126        COMPUTE WS-IMPORTE-REPARTOS =
070126*                               (((X350JPX4-PERIOD   ) -
070126*                                  X350JPX4-GASTOS           -
070126*                                  X350JPX4-SINIES             ) *
070126*                                  X350JPX5-PRIMA-ANUAL        ) /
070126*                                          X350JPX4-PRIMA-ANUAL

070126               IF WS-FECHA-HASTA-NR NOT EQUAL
070126                  R350T000-FFINAL
070126
070126                  COMPUTE WS-IMPORTE-REPARTOS =
070126                                   (((X350JPX4-PERIOD ) -
070126                                      X350JPX4-GASTOS        -
070126                                      X350JPX4-SINIES          ) *
070126                                      X350JPX5-PRIMA-ANUAL     ) /
070126                                              X350JPX4-PRIMA-ANUAL
070126               ELSE
070126                  COMPUTE WS-IMPORTE-REPARTOS =
070126                                   (((X350JPX4-PERIOD-NR ) -
070126                                      X350JPX4-GASTOS-NR        -
070126                                      X350JPX4-SINIES-NR       ) *
070126                                      X350JPX5-PRIMA-ANUAL     ) /
070126                                              X350JPX4-PRIMA-ANUAL
070126               END-IF


051124*          IF X350JPX3-PAGADOR = '0'
051124*             COMPUTE WS-IMPORTE-REPARTOS    =
051124*                                  ((X350JPX4-PERIOD-NR -
051124*                                    X350JPX4-SINIES-NR  -
051124*                                    X350JPX4-GASTOS-NR) *
051124*                                    X350JPX5-PRIMA-ANUAL) /
051124*                                    X350JPX4-PRIMA-ANUAL
051124*          END-IF
      *D*
      *           DISPLAY 'REPARTOS ' WS-IMPORTE-REPARTOS
      *F*
                 END-IF


                  MOVE WS-IMPORTE-REPARTOS TO X350JPX6-INGRESOS-GASTOS

                  IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
                     MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
                  END-IF

                  COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
                                               = WS-IMPORTE-REPARTOS

                ELSE
      *D*
      *        DISPLAY 'ME VOY POR ELSE SI SUSCRI-NR'
      *        DISPLAY 'X350JPX4-KPOLIZA       ' X350JPX4-KPOLIZA
      *        DISPLAY 'X350JPX3-KPOLIZA       ' X350JPX3-KPOLIZA
      *        DISPLAY 'X350JPX3-KSUBPOL       ' X350JPX3-KSUBPOL
      *        DISPLAY 'X350JPX4-KSUBPOL       ' X350JPX4-KSUBPOL
      *        DISPLAY 'X350JPX3-KGRUEMP       ' X350JPX3-KGRUEMP
      *        DISPLAY 'X350JPX4-KGRUEMP       ' X350JPX4-KGRUEMP
      *F*
                  OPEN INPUT ENTRADA-TASE-NE
                  PERFORM 2703-LEER-TOTALES-ASEGURADO-NE
                    UNTIL X350JPX3-KPOLIZA EQUAL
                          X350JPX4-KPOLIZA AND
                          X350JPX3-KSUBPOL EQUAL
                          X350JPX4-KSUBPOL AND
                          X350JPX3-KGRUEMP EQUAL
                          X350JPX4-KGRUEMP
                  CLOSE ENTRADA-TASE-NE

                  OPEN INPUT ENTRADA-TASE-CP
                  PERFORM 2703-LEER-TOTALES-ASEGURADO-CP
                    UNTIL X350JPX3-KPOLIZA EQUAL
                          X350JPX5-KPOLIZA AND
                          X350JPX3-KSUBPOL EQUAL
                          X350JPX5-KSUBPOL
                  CLOSE ENTRADA-TASE-CP

                  IF X350JPX5-PRIMA-ANUAL <= ZEROES
                     MOVE ZEROES TO WS-IMPORTE-REPARTOS
                  ELSE
                     COMPUTE WS-IMPORTE-REPARTOS = ((WS-IMPORTE-PERIOD -
MF001 *                                         WS-IMPORTE-206-NR   -
MF001                                           TB-ACUMULADO-206 (1) -
                                                WS-IMP-GASTOS) *
                                                X350JPX5-PRIMA-ANUAL) /
                                                X350JPX5-PRIMA-ANUAL
                  END-IF

                  MOVE WS-IMPORTE-REPARTOS TO X350JPX6-INGRESOS-GASTOS

                  IF X350JPX6-INGRESOS-GASTOS NOT NUMERIC
                     MOVE ZEROES TO X350JPX6-INGRESOS-GASTOS
                  END-IF

                  COMPUTE RSAL-INGRESOS-GASTOS ROUNDED
                                              =  WS-IMPORTE-REPARTOS
                END-IF

                COMPUTE WS-NETA-COASEGUR ROUNDED
                                         = (WS-IMPORTE-REPARTOS *
                                            X350JPX3-PCOASE) / 100

                MOVE WS-NETA-COASEGUR         TO X350JPX6-NETA-COASEGUR

                IF X350JPX6-NETA-COASEGUR NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-NETA-COASEGUR
                END-IF

                COMPUTE RSAL-NETA-COASEGUR ROUNDED
                                           = WS-NETA-COASEGUR

                MOVE RSAL-P-PB-APLICAR  TO AUX-PB-TECNICA

                COMPUTE RSAL-PB-TECNICA ROUNDED
                                        = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100

                COMPUTE X350JPX6-PB-TECNICA = (WS-IMPORTE-REPARTOS *
                                           AUX-PB-TECNICA) / 100

                IF X350JPX6-PB-TECNICA NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-PB-TECNICA
                END-IF

                MOVE X350JPX3-PCOASE          TO AUX-PCOASE

                COMPUTE X350JPX6-DEV-NETCOASE = (WS-NETA-COASEGUR *
                                       AUX-PB-TECNICA) / 100

                IF X350JPX6-DEV-NETCOASE < ZEROES
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                   MOVE ZEROES TO RSAL-DEV-NETCOASE
                ELSE
                   COMPUTE RSAL-DEV-NETCOASE ROUNDED
                                          = (WS-NETA-COASEGUR *
                                          AUX-PB-TECNICA) / 100
                END-IF

                IF X350JPX6-DEV-NETCOASE NOT NUMERIC
                   MOVE ZEROES TO X350JPX6-DEV-NETCOASE
                END-IF

           IF RSAL-PGLOBAL = '0'
RF001         MOVE X350JPX3-KPOLIZA         TO RSAL-POLIZA-REF1
              MOVE '/'                      TO RSAL-SEPARADOR
              MOVE '0000'                   TO RSAL-SUBPOL-REF1
           END-IF

           IF RSAL-POLIZA-REF1 = '0000000'
              MOVE SPACES TO RSAL-POLIZA-REF1
              MOVE SPACES TO RSAL-SEPARADOR
              MOVE SPACES TO RSAL-SUBPOL-REF1
           END-IF

           PERFORM 2210-RECUPERAR-UOA-NR

           PERFORM 2300-OBTENER-NEW-GAP

           MOVE RSAL-GAP     TO X350JPX6-GAP

LUNA       MOVE RSAL-FCIERREA TO WS-FECSIN-ACIERRE
LUNA       MOVE RSAL-FCIERREM TO WS-FECSIN-MCIERRE
LUNA       MOVE RSAL-FCIERRED TO WS-FECSIN-DCIERRE

LUNN       IF WS-RENOVADA = 'N' AND WS-ANTPBAN = 'S' AND RSAL-LTA = 'PB'

JOSEN *           IF WS-FECSIN-DESDE > WS-FECSIN-HASTA
JOSEN *              MOVE WS-FECSIN-HASTA TO WS-FECSIN-DESDE-PB
                  IF WS-FECSIN-HASTA  > WS-FECSIN-DESDE-PB
                     CONTINUE
                  ELSE
JOSEN                MOVE WS-FECSIN-HASTA TO WS-FECSIN-DESDE-PB
                  END-IF

      *D*
      *         DISPLAY 'ENTRO IF              '
      *F*
JOSEN *           ELSE
JOSEN *              MOVE WS-FECSIN-HASTA TO WS-FECSIN-DESDE-PB
LUNN                 MOVE WS-FECSIN-DESDE-PB TO WS-FECSIN-DESDE
JOSEN                ADD 1 TO WS-FECSIN-AHASTA
      *           END-IF
LUNN       END-IF

LUNA
LUNA
JOSEN      IF WS-FECSIN-DESDE > WS-FECSIN-CIERRE
              MOVE WS-FECSIN-ACIERRE TO WS-FECSIN-ADESDE
              MOVE WS-FECSIN-DESDE TO RSAL-FECSIN-DESDE
           ELSE
LUNA          MOVE WS-FECSIN-DESDE TO RSAL-FECSIN-DESDE
           END-IF

LUNA       IF WS-FECSIN-CIERRE < WS-FECSIN-HASTA
LUNA          MOVE WS-FECSIN-CIERRE TO RSAL-FECSIN-HASTA
LUNA       ELSE
LUNA          MOVE WS-FECSIN-HASTA TO RSAL-FECSIN-HASTA
LUNA       END-IF

JOSEN

      *D*
      *    DISPLAY 'RSAL-FECSIN-DESDE     ' RSAL-FECSIN-DESDE
      *    DISPLAY 'RSAL-FECSIN-HASTA     ' RSAL-FECSIN-HASTA
      *F*

           WRITE REG-SALIDA1 FROM RSALIDA
           WRITE REG-SALIDA3 FROM X350JPX6

           MOVE WS-POLIZA-2 TO WS-POLIZA-2-ANT
           MOVE WS-SUBPOL-2 TO WS-SUBPOL-2-ANT
      *    MOVE WS-FECHA-DESDE-NR TO X350JXX2-FRENO-DESDE
      *    MOVE WS-FECHA-HASTA-NR TO X350JXX2-FRENO-HASTA

           INITIALIZE RSALIDA
                      X350JPX6
           .
      *
      ******************************************************************10650099
       5100-ESCRIBIR-PRIMERA-LINEA.                                     10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5100-ESCRIBIR-PRIMERA-LINEA'
      *F*
           INITIALIZE REG-SALIDA1
      *
           STRING 'NOMBRE DE LA CADENA: '  WS-CADENA                    09250099
                  ' ;FECHA DE EJECUCION: ' WS-FECHA-FORMATEADA          09270099
                  ' ;FECHA DE CIERRE   : ' R350T000-FDFINAL '-'         09270099
                                           R350T000-FMFINAL '-'
                                           R350T000-FAFINAL
                                   DELIMITED BY SIZE                    09290099
                                   INTO PRIMERA-LINEA                   09300099
      *
           WRITE REG-SALIDA1 FROM PRIMERA-LINEA
           .
      *
      ******************************************************************10650099
       2400-ESCRIBIR-ERRORES.                                           10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5200-ESCRIBIR-SEGUNDA-LINEA'
      *F*
           INITIALIZE REG-SALIDA2
      *
           WRITE REG-SALIDA2 FROM X350J000
           .
      *
      ******************************************************************10650099
       5200-ESCRIBIR-SEGUNDA-LINEA.                                     10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5200-ESCRIBIR-SEGUNDA-LINEA'
      *F*
           INITIALIZE REG-SALIDA1
      *
           WRITE REG-SALIDA1 FROM SEGUNDA-LINEA
           .
      *
      ******************************************************************10650099
FICOL  2702-LLAMADA-FICOL.                                              10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2702-LLAMADA-FICOL'
      *    DISPLAY 'X350JXX2-KSG-POLSUB ' X350JXX2-KSG-POLSUB
      *F*
                  OPEN INPUT ENTRADA-FICOL
                  MOVE 'N'  TO SW-FIN-FICOL
                  PERFORM 2799-LEER-FICOL
                    UNTIL X350JXX2-KSG-POLSUB EQUAL
                          X350FICO-KPOLIZA OR
                          SW-SI-FINAL-FICOL
                  CLOSE ENTRADA-FICOL

               MOVE X350FICO-BAREMO          TO AUX-CCUADROBAREMO-S
               MOVE X350FICO-REAJUS-PB       TO AUX-PCALCULOPB-S
      *D*
      *    DISPLAY 'X350FICO-GASTOS     ' X350FICO-GASTOS
      *    DISPLAY 'AUX-PGASTOS-S       ' AUX-PGASTOS-S
      *F*
               MOVE X350FICO-GASTOS          TO AUX-PGASTOS-S
               MOVE X350FICO-BONIFICACION    TO AUX-REAJUSTE-PB
               MOVE X350FICO-REAJUS-TIPO     TO AUX-TIP-REAJUSTE
               MOVE X350FICO-ANOS-PERDIDAS   TO AUX-NANIOPER
               MOVE X350FICO-ANOS-BENEFICIOS TO AUX-NANIOBEN
PBTEC          MOVE X350FICO-CCOBON-REAJUS   TO AUX-CCOBON-REAJUS
      *D*
      *        DISPLAY ' KSG-POLSUB      ' X350JXX2-KSG-POLSUB
      *        DISPLAY ' KPOLIZA         ' X350FICO-KPOLIZA
      *        DISPLAY ' BAREMO          ' X350FICO-BAREMO
      *        DISPLAY ' REAJUS-PB       ' X350FICO-REAJUS-PB
      *        DISPLAY ' GASTOS          ' X350FICO-GASTOS
      *        DISPLAY ' BONIFICACION    ' X350FICO-BONIFICACION
      *        DISPLAY ' REAJUS-TIPO     ' X350FICO-REAJUS-TIPO
      *        DISPLAY ' ANOS-PERDIDAS   ' X350FICO-ANOS-PERDIDAS
      *        DISPLAY ' ANOS-BENEFICIOS ' X350FICO-ANOS-BENEFICIOS
      *        DISPLAY ' CCOBON-REAJUSTE ' X350FICO-CCOBON-REAJUS
      *F*
                     IF AUX-CCUADROBAREMO-S EQUAL ZEROS
                       MOVE SPACES TO AUX-CCUADROBAREMO-S
                       MOVE 'POL CON BAREMO 0' TO X350J000-DESC-ERROR
                       MOVE 'B350JA12'          TO X350J000-ID-PROGRAMA
                       MOVE 'B350JA12'          TO X350J000-ID-MODULO
                       MOVE X350JXX2-KPOLIZA    TO X350J000-KPOLIZA
                       MOVE X350JXX2-KSUBPOL    TO X350J000-KSUBPOLIZA
                       MOVE 'W'                 TO X350J000-NIVEL
                       MOVE '01'                TO X350J000-ID-ERROR-MOD
                       PERFORM 2400-ESCRIBIR-ERRORES
                     END-IF

           .
      *
      ******************************************************************10650099
LTA    5200-GESTION-LTA.                                                10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5200-GESTION-LTA'
      *F*
      *    PERFORM 5210-LLAMADA-LTP


      *    PERFORM 5100-GRABAR-SALIDA-SUSCRIP

      *    IF X350JXX2-FRENO-HASTA   >  R350T000-FFINAL
      *       MOVE 'S'                   TO RSAL-RENOVADA
      *       MOVE 'S'                   TO WS-RENOVADA
      *    ELSE
      *       MOVE 'N'                   TO RSAL-RENOVADA
      *       MOVE 'N'                   TO WS-RENOVADA
      *    END-IF

      *    IF WS-RENOVADA = 'S'
      *       PERFORM 5220-LLAMADA-LTB
      *    ELSE
      *       PERFORM 5230-LLAMADA-LTB-NR
      *    END-IF

           IF SW-SI-LTP

             PERFORM 2400-ACUMULAR-MOVS-LTP
               UNTIL WS-CLAVE1 NOT EQUAL WS-CLAVE1-ANT

0124       IF SW-SI-LTA-FINAL AND SW-SW-LTPA EQUAL 'N'
                              AND WS-RENOVADA-LTA EQUAL 'N'
      *D*
      *    DISPLAY 'entro if'
      *F*
0124          PERFORM 2702-LLAMADA-FICOL
0124          PERFORM OBTENER-RB03631-RR
0124          PERFORM 2702-CONSULTAR-BAREMO
0124          PERFORM 2600-LINEA-MVTOS-FINAL-PBANT
0124          PERFORM 5000-GRABAR-SALIDA
              MOVE X350JXX2                      TO X350JPX3
0124          PERFORM 2100-RECUPERAR-DATOS-NO-RENO
0124          PERFORM 5100-GRABAR-SALIDA-SI-FINAL
0124          MOVE WS-FRENO-DESDE-PLURI-ACTUAL TO WS-FECSIN-DESDE
0124          MOVE WS-FECHA-HASTA-TRAMO        TO WS-FECSIN-HASTA

RF001         IF WS-FECHA-HASTA-TRAMO EQUAL ZEROES
RF001            MOVE WS-FRENO-HASTA-PLURI-ACTUAL TO WS-FECSIN-HASTA
RF001            MOVE WS-FRENO-HASTA-PLURI-ACTUAL
                                                 TO WS-FECHA-HASTA-TRAMO
RF001         END-IF
0124       END-IF

             PERFORM 2600-LINEA-MVTOS-LTA
             PERFORM 5000-GRABAR-SALIDA-LTP-1
             PERFORM 5000-GRABAR-SALIDA-LTP-2

RF001            IF SW-SI-LTPA AND SW-SI-LTBA
PLURA              PERFORM 5211-GESTION-MOVIMIENTOS-ANT
PLURA              PERFORM 5000-GRABAR-SALIDA-LTP-A
RF001              PERFORM 5000-GRABAR-SALIDA-LTB-A
RF001            ELSE
                    IF SW-SI-LTPA
PLURA                 PERFORM 5211-GESTION-MOVIMIENTOS-ANT
PLURA                 PERFORM 5000-GRABAR-SALIDA-LTP-A
                    END-IF
RF001               IF SW-SI-LTBA AND SW-NO-LTB
RF001                 PERFORM 5211-GESTION-MOVIMIENTOS-ANT
RF001                 PERFORM 5000-GRABAR-SALIDA-LTB-A
RF001               END-IF

                 END-IF
           END-IF

           IF SW-SI-LTB
      *D*
      *      DISPLAY 'HAY BONO ' WS-RENOVADA
      *      DISPLAY 'X350JXX2-KPOLIZA ' X350JXX2-KPOLIZA
      *      DISPLAY 'X350JPX3-KPOLIZA ' X350JPX3-KPOLIZA
      *      DISPLAY 'WS-RENOVADA ' WS-RENOVADA
      *F*
1412***      IF WS-RENOVADA = 'S' AND SW-SI-LTP
1412         IF SW-SI-LTP
      *         PERFORM 5211-GESTION-MOVIMIENTOS-LTB
                PERFORM 5000-GRABAR-SALIDA-LTB-P
BONOA           IF SW-SI-LTBA
                   PERFORM 5211-GESTION-MOVIMIENTOS-ANT
      *D*
      *    DISPLAY 'SALIDA ACUMULAR ANT ' WS-IMPORTE-205
      *F*
                   PERFORM 5000-GRABAR-SALIDA-LTB-A
                END-IF
             ELSE
                IF WS-RENOVADA = 'N'
                   PERFORM 5211-GESTION-MOVIMIENTOS-LTB
                   PERFORM 5000-GRABAR-SALIDA-LTB-NR

BONOA              IF SW-SI-LTBA
                      PERFORM 5211-GESTION-MOVIMIENTOS-ANT
      *D*
      *    DISPLAY 'SALIDA ACUMULAR ANT ' WS-IMPORTE-205
      *F*
                      PERFORM 5000-GRABAR-SALIDA-LTB-A
                   END-IF
                ELSE
                   PERFORM 5211-GESTION-MOVIMIENTOS-LTB
                   PERFORM 5000-GRABAR-SALIDA-LTB
BONOA           IF SW-SI-LTBA
                   PERFORM 5211-GESTION-MOVIMIENTOS-ANT
      *D*
      *    DISPLAY 'SALIDA ACUMULAR ANT ' WS-IMPORTE-205
      *F*
                   PERFORM 5000-GRABAR-SALIDA-LTB-A
                END-IF
                END-IF
             END-IF
           END-IF
           .
      *
      ******************************************************************10650099
LTA    5210-LLAMADA-LTP.                                                10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2110-LLAMADA-LTP'
      *F*

           INITIALIZE X350M004-DATOS-ENTRADA
                      X350M004
                      AUX-KGRUPEMP-LTP

PLURIN          IF X350JXX2-FRENO-HASTA   >  R350T000-FFINAL
PLURIN             MOVE 'S'                   TO WS-RENOVADA-LTA
PLURIN          ELSE
PLURIN             MOVE 'N'                   TO WS-RENOVADA-LTA
PLURIN          END-IF


PLURIN     IF WS-RENOVADA-LTA EQUAL 'N'
PLURIN        MOVE X350JXX2-FRENO-ADESDE TO WS-FRENO-DESDE-APLURI
PLURIN        MOVE X350JXX2-FRENO-AHASTA TO WS-FRENO-HASTA-APLURI
PLURIN        ADD 1 TO WS-FRENO-DESDE-APLURI
PLURIN        ADD 1 TO WS-FRENO-HASTA-APLURI
PLURIN     END-IF

           SET SW-NO-LTP  TO TRUE
RF001      SET SW-NO-LTA-SOLO-ANT-P TO TRUE
           MOVE X350JXX2-KPOLIZA      TO X350M004-KPOLIZA-E
           MOVE X350JXX2-KSUBPOL      TO X350M004-KSUBPOL-E
           MOVE X350JXX2-KGRUEMP      TO AUX-KGRUPEMP-LTP
           INSPECT AUX-KGRUPEMP-LTP    REPLACING ALL " " BY "0"
           MOVE AUX-KGRUPEMP-LTP      TO X350M004-KGRUPEMP-E

PLURIN     IF WS-RENOVADA-LTA EQUAL 'N'
PLURIN        MOVE WS-FRENO-DESDE-APLURI TO X350M004-FDESA-E
PLURIN     ELSE
              MOVE X350JXX2-FRENO-ADESDE TO X350M004-FDESA-E
PLURIN     END-IF

           MOVE X350JXX2-FRENO-MDESDE TO X350M004-FDESM-E
           MOVE X350JXX2-FRENO-DDESDE TO X350M004-FDESD-E

PLURIN     IF WS-RENOVADA-LTA EQUAL 'N'
PLURIN        MOVE WS-FRENO-HASTA-APLURI TO X350M004-FHASA-E
PLURIN     ELSE
              MOVE X350JXX2-FRENO-AHASTA TO X350M004-FHASA-E
PLURIN     END-IF

           MOVE X350JXX2-FRENO-MHASTA TO X350M004-FHASM-E
           MOVE X350JXX2-FRENO-DHASTA TO X350M004-FHASD-E
           MOVE 'P'                   TO X350M004-CTIPO-E

      *D*
      *    DISPLAY 'X350M004-KPOLIZA-E  ' X350M004-KPOLIZA-E
      *    DISPLAY 'X350M004-KSUBPOL-E  ' X350M004-KSUBPOL-E
      *    DISPLAY 'X350M004-KGRUPEMP-E ' X350M004-KGRUPEMP-E
      *    DISPLAY 'X350JXX2-FRENO-DESDE ' X350JXX2-FRENO-DESDE
      *    DISPLAY 'X350JXX2-FRENO-HASTA ' X350JXX2-FRENO-HASTA
      *    DISPLAY 'X350M004-FDESA-E    ' X350M004-FDESA-E
      *    DISPLAY 'X350M004-FDESM-E    ' X350M004-FDESM-E
      *    DISPLAY 'X350M004-FDESD-E    ' X350M004-FDESD-E
      *    DISPLAY 'X350M004-FHASA-E    ' X350M004-FHASA-E
      *    DISPLAY 'X350M004-FHASM-E    ' X350M004-FHASM-E
      *    DISPLAY 'X350M004-FHASD-E    ' X350M004-FHASD-E
      *    DISPLAY 'X350M004-CTIPO-E    ' X350M004-CTIPO-E
      *F*
RF001****  CALL B350M004           USING WS-X350M004
RF001      CALL B350M004 USING WS-X350M006

           EVALUATE X350M004-CRETORNO
               WHEN '00'
      *D*
      *    DISPLAY 'SI LTP'
      *    DISPLAY 'X350M004-PREAJPB-PS ' X350M004-PREAJPB-PS
      *    DISPLAY 'X350M004-PGASTOS-PS ' X350M004-PGASTOS-PS
      *    DISPLAY 'X350M004-TRAMO-DESDE ' X350M004-FDESA-PS
      *                                    X350M004-FDESM-PS
      *                                    X350M004-FDESD-PS
      *F*
210524     MOVE X350M004-PREAJPB-PS TO WS-PRREAPB-PS-ACTUAL
210524     MOVE X350M004-PGASTOS-PS TO WS-PGASTOS-PS-ACTUAL
130624     MOVE X350M004-KGRUPEMP-PS TO WS-KGRUPEMP-PS-ACTUAL
PLURIN         MOVE X350M004-FDESA-PS    TO WS-FRENO-DESDE-APLURI-ACTUAL
PLURIN         MOVE X350M004-FDESM-PS    TO WS-FRENO-DESDE-MPLURI-ACTUAL
PLURIN         MOVE X350M004-FDESD-PS    TO WS-FRENO-DESDE-DPLURI-ACTUAL
RF001          MOVE X350M004-FHASA-PS    TO WS-FRENO-HASTA-APLURI-ACTUAL
RF001          MOVE X350M004-FHASM-PS    TO WS-FRENO-HASTA-MPLURI-ACTUAL
RF001          MOVE X350M004-FHASD-PS    TO WS-FRENO-HASTA-DPLURI-ACTUAL

      *D*
      *    DISPLAY 'X350JXX2-FRENO-AHASTA ' X350JXX2-FRENO-AHASTA
      *    DISPLAY 'X350M004-FDESA-PS     ' X350M004-FDESA-PS
      *    DISPLAY 'WS-RENOVADA-LTA       ' WS-RENOVADA-LTA
      *F*
RF001      IF X350M004-FDESA-PS > 0

PLURIN         IF WS-RENOVADA-LTA EQUAL 'N' AND
PLURIN            X350JXX2-FRENO-AHASTA = X350M004-FDESA-PS
PLURIN            SET SW-SI-LTA-FINAL TO TRUE
PLURIN         ELSE
PLURIN
PLURIN            IF WS-RENOVADA-LTA EQUAL 'N'
PLURIN               SET SW-SI-LTA-MITAD TO TRUE
PLURIN            END-IF
PLURIN         END-IF
RF001      ELSE
RF001          CONTINUE
RF001      END-IF

PLURA               PERFORM 5221-BUSCAR-TRAMO-ANTERIOR
RF001 *D*
RF001 *    DISPLAY 'SW-NO-LTB ' SW-SW-LTB
RF001 *    DISPLAY 'SW-SW-LTBA' SW-SW-LTBA
RF001 *    DISPLAY 'SW-SW-LTP ' SW-SW-LTP
RF001 *    DISPLAY 'SW-SW-LTPA ' SW-SW-LTPA
RF001 *F*
      *D*
      *    DISPLAY 'SI LTP'
      *F*
           IF X350M004-CRETORNO = '00'
RF001*******  MOVE X350M004-PREAJPB-PS TO WS-PRREAPB-PS-ANTERIOR
RF001******** MOVE X350M004-PGASTOS-PS TO WS-PGASTOS-PS-ANTERIOR
RF001******   MOVE X350M004-KGRUPEMP-PS TO WS-KGRUPEMP-PS-ANTERIOR
RF001         MOVE X350M004-PREAJPB-PS-ANT TO WS-PRREAPB-PS-ANTERIOR
RF001         MOVE X350M004-PGASTOS-PS-ANT TO WS-PGASTOS-PS-ANTERIOR
RF001         MOVE X350M004-KGRUPEMP-PS-ANT TO WS-KGRUPEMP-PS-ANTERIOR
           END-IF

RF001*******        PERFORM 5222-BUSCAR-TRAMO-FUTURO
RF001********
RF001******   X350M004-CRETORNO = '00'
RF001*******  MOVE X350M004-PREAJPB-PS TO WS-PRREAPB-PS-FUTURO
RF001*******  MOVE X350M004-PGASTOS-PS TO WS-PGASTOS-PS-FUTURO
RF001******** MOVE X350M004-KGRUPEMP-PS TO WS-KGRUPEMP-PS-FUTURO
RF001******    END-IF

      *D*
      *    DISPLAY 'X350M004-FDESA-PS ' X350M004-FDESA-PS
      *    DISPLAY 'X350M004-FHASA-PS-ANT ' X350M004-FHASA-PS-ANT
      *F*

RF001*******       IF (X350M004-FDESA-PS EQUAL SPACES OR LOW-VALUES) AND
RF001*********         X350M004-FHASA-PS-ANT > 2000
RF001**********    IF (X350M004-FDESA-PS EQUAL SPACES OR LOW-VALUES) AND
RF001***********       X350M004-FHASA-PS-ANT > 2000  OR
RF001***********   (X350JXX2-KPOLIZA  NOT EQUAL X350M004-KPOLIZA-PS AND
RF001************   X350JXX2-KSUBPOL  NOT EQUAL X350M004-KSUBPOL-PS)

RF001              IF X350M004-FDESA-PS > 0
RF001                  SET SW-SI-LTP TO TRUE
RF001              END-IF
      *D*
      *    DISPLAY '-DATOS-PLUR-ACTUAL ' X350M004-DATOS-PLURIANUAL
      *    DISPLAY '-DATOS-PLUR-ANTERIO' X350M004-DATOS-PLURIANUAL-ANT
      *F*
      *D*
      *    DISPLAY 'X350M004-FDESA-PS ' X350M004-FDESA-PS
      *    DISPLAY 'X350M004-FDESM-PS ' X350M004-FDESM-PS
      *    DISPLAY 'X350M004-FDESD-PS ' X350M004-FDESD-PS
      *    DISPLAY 'X350M004-FHASA-PS ' X350M004-FHASA-PS
      *    DISPLAY 'X350M004-FHASM-PS ' X350M004-FHASM-PS
      *    DISPLAY 'X350M004-FHASD-PS ' X350M004-FHASD-PS
      *    DISPLAY 'X350M004-PREAJPB-PS ' X350M004-PREAJPB-PS
      *    DISPLAY 'X350M004-PGASTOS-PS ' X350M004-PGASTOS-PS
      *F*
                    MOVE X350M004-FDESA-PS TO WS-ANNO-LTA-D
                    MOVE X350M004-FDESM-PS TO WS-MES-LTA-D
                    MOVE X350M004-FDESD-PS TO WS-DIA-LTA-D

                    MOVE X350M004-FHASA-PS TO WS-ANNO-LTA-H
                    MOVE X350M004-FHASM-PS TO WS-MES-LTA-H
                    MOVE X350M004-FHASD-PS TO WS-DIA-LTA-H

               WHEN OTHER

                    MOVE '0000000'         TO X350M004-KPOLIZA-E
                    MOVE '00000'           TO X350M004-KSUBPOL-E

RF001******         CALL B350M004           USING WS-X350M004
RF001               CALL B350M004 USING WS-X350M006

                    EVALUATE X350M004-CRETORNO
                        WHEN '00'
210524     MOVE X350M004-PREAJPB-PS TO WS-PRREAPB-PS-ACTUAL
210524     MOVE X350M004-PGASTOS-PS TO WS-PGASTOS-PS-ACTUAL
130624     MOVE X350M004-KGRUPEMP-PS TO WS-KGRUPEMP-PS-ACTUAL
PLURIN         MOVE X350M004-FDESA-PS    TO WS-FRENO-DESDE-APLURI-ACTUAL
PLURIN         MOVE X350M004-FDESM-PS    TO WS-FRENO-DESDE-MPLURI-ACTUAL
PLURIN         MOVE X350M004-FDESD-PS    TO WS-FRENO-DESDE-DPLURI-ACTUAL
RF001          MOVE X350M004-FHASA-PS    TO WS-FRENO-HASTA-APLURI-ACTUAL
RF001          MOVE X350M004-FHASM-PS    TO WS-FRENO-HASTA-MPLURI-ACTUAL
RF001          MOVE X350M004-FHASD-PS    TO WS-FRENO-HASTA-DPLURI-ACTUAL

      *D*
      *    DISPLAY 'WS-RENOVADA-LTA   ' WS-RENOVADA-LTA
      *    DISPLAY 'X350JXX2-FRENO-AHASTA ' X350M004-FHASA-PS-ANT
      *    DISPLAY 'X350M004-FDESA-PS     ' X350M004-FDESA-PS
      *F*
RF001      IF X350M004-FDESA-PS > 0

PLURIN         IF WS-RENOVADA-LTA EQUAL 'N' AND
PLURIN            X350JXX2-FRENO-AHASTA = X350M004-FDESA-PS
PLURIN            SET SW-SI-LTA-FINAL TO TRUE
PLURIN         ELSE
PLURIN
PLURIN            IF WS-RENOVADA-LTA EQUAL 'N'
PLURIN               SET SW-SI-LTA-MITAD TO TRUE
PLURIN            END-IF
PLURIN         END-IF
RF001      ELSE
RF001          CONTINUE
RF001      END-IF

PLURA               PERFORM 5221-BUSCAR-TRAMO-ANTERIOR

RF001 *D*
RF001 *    DISPLAY 'SW-NO-LTB ' SW-SW-LTB
RF001 *    DISPLAY 'SW-SW-LTBA' SW-SW-LTBA
RF001 *    DISPLAY 'SW-SW-LTP ' SW-SW-LTP
RF001 *    DISPLAY 'SW-SW-LTPA ' SW-SW-LTPA
RF001 *F*
           IF X350M004-CRETORNO = '00'
RF001*******  MOVE X350M004-PREAJPB-PS TO WS-PRREAPB-PS-ANTERIOR
RF001******** MOVE X350M004-PGASTOS-PS TO WS-PGASTOS-PS-ANTERIOR
RF001******   MOVE X350M004-KGRUPEMP-PS TO WS-KGRUPEMP-PS-ANTERIOR
RF001         MOVE X350M004-PREAJPB-PS-ANT TO WS-PRREAPB-PS-ANTERIOR
RF001         MOVE X350M004-PGASTOS-PS-ANT TO WS-PGASTOS-PS-ANTERIOR
RF001         MOVE X350M004-KGRUPEMP-PS-ANT TO WS-KGRUPEMP-PS-ANTERIOR
           END-IF

RF001*******        PERFORM 5222-BUSCAR-TRAMO-FUTURO
RF001********
RF001******         IF X350M004-CRETORNO = '00'
RF001*******          MOVE X350M004-PREAJPB-PS TO WS-PRREAPB-PS-FUTURO
RF001*******          MOVE X350M004-PGASTOS-PS TO WS-PGASTOS-PS-FUTURO
RF001********         MOVE X350M004-KGRUPEMP-PS TO WS-KGRUPEMP-PS-FUTURO
RF001******         END-IF

RF001 *******      IF (X350M004-FDESA-PS EQUAL SPACES OR LOW-VALUES) AND
RF001 *********        X350M004-FHASA-PS-ANT > 2000 OR
RF001 **********   IF (X350M004-FDESA-PS EQUAL SPACES OR LOW-VALUES) AND
RF001 ***********      X350M004-FHASA-PS-ANT > 2000  OR
RF001 ***********  (X350JXX2-KPOLIZA  NOT EQUAL X350M004-KPOLIZA-PS AND
RF001 ************  X350JXX2-KSUBPOL  NOT EQUAL X350M004-KSUBPOL-PS)
RF001              IF X350M004-FDESA-PS > 0
RF001                  SET SW-SI-LTP TO TRUE
RF001              END-IF
      *D*
      *    DISPLAY '-DATOS-PLUR-ACTUAL ' X350M004-DATOS-PLURIANUAL
      *    DISPLAY '-DATOS-PLUR-ANTERIO' X350M004-DATOS-PLURIANUAL-ANT
      *F*

                        MOVE X350M004-FDESA-PS TO WS-ANNO-LTA-D
                        MOVE X350M004-FDESM-PS TO WS-MES-LTA-D
                        MOVE X350M004-FDESD-PS TO WS-DIA-LTA-D

                        MOVE X350M004-FHASA-PS TO WS-ANNO-LTA-H
                        MOVE X350M004-FHASM-PS TO WS-MES-LTA-H
                        MOVE X350M004-FHASD-PS TO WS-DIA-LTA-H

                    WHEN OTHER
                    SET SW-NO-LTP TO TRUE
                   END-EVALUATE
           END-EVALUATE
           .
      ******************************************************************10650099
LTA    5220-LLAMADA-LTB.                                                10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '2110-LLAMADA-LTB'
      *F*

           INITIALIZE X350M004-DATOS-ENTRADA
                      AUX-KGRUPEMP-LTB

           SET SW-NO-LTB  TO TRUE
RF001      SET SW-NO-LTA-SOLO-ANT-B TO TRUE
           MOVE X350JXX2-KPOLIZA      TO X350M004-KPOLIZA-E
           MOVE X350JXX2-KSUBPOL      TO X350M004-KSUBPOL-E
           MOVE X350JXX2-KGRUEMP      TO AUX-KGRUPEMP-LTB
           INSPECT AUX-KGRUPEMP-LTB    REPLACING ALL " " BY "0"
           MOVE AUX-KGRUPEMP-LTB      TO X350M004-KGRUPEMP-E
PLURIN     IF WS-RENOVADA-LTA EQUAL 'N'
PLURIN        MOVE WS-FRENO-DESDE-APLURI TO X350M004-FDESA-E
PLURIN     ELSE
              MOVE X350JXX2-FRENO-ADESDE TO X350M004-FDESA-E
PLURIN     END-IF
           MOVE X350JXX2-FRENO-MDESDE TO X350M004-FDESM-E
           MOVE X350JXX2-FRENO-DDESDE TO X350M004-FDESD-E
PLURIN     IF WS-RENOVADA-LTA EQUAL 'N'
PLURIN        MOVE WS-FRENO-HASTA-APLURI TO X350M004-FHASA-E
PLURIN     ELSE
              MOVE X350JXX2-FRENO-AHASTA TO X350M004-FHASA-E
PLURIN     END-IF
           MOVE X350JXX2-FRENO-MHASTA TO X350M004-FHASM-E
           MOVE X350JXX2-FRENO-DHASTA TO X350M004-FHASD-E
           MOVE 'B'                   TO X350M004-CTIPO-E
RF001***** CALL B350M004           USING WS-X350M004
RF001      CALL B350M004 USING WS-X350M006

           EVALUATE X350M004-CRETORNO
               WHEN '00'

210524     MOVE X350M004-PPBBONO-BS  TO WS-PRREAPB-BS-ACTUAL
210524     MOVE X350M004-PGASBONO-BS TO WS-PGASTOS-BS-ACTUAL
130624     MOVE X350M004-KGRUPEMP-BS TO WS-KGRUPEMP-BS-ACTUAL
PLURIN         MOVE X350M004-FDESA-BS TO    WS-FRENO-DESDE-ABONO-ACTUAL
PLURIN         MOVE X350M004-FDESM-BS TO    WS-FRENO-DESDE-MBONO-ACTUAL
PLURIN         MOVE X350M004-FDESD-BS TO    WS-FRENO-DESDE-DBONO-ACTUAL
RF001          MOVE X350M004-FHASA-BS TO    WS-FRENO-HASTA-ABONO-ACTUAL
RF001          MOVE X350M004-FHASM-BS TO    WS-FRENO-HASTA-MBONO-ACTUAL
RF001          MOVE X350M004-FHASD-BS TO    WS-FRENO-HASTA-DBONO-ACTUAL

RF001      IF X350M004-FDESA-BS > 0

PLURIN         IF WS-RENOVADA-LTA EQUAL 'N' AND
PLURIN            X350JXX2-FRENO-AHASTA = X350M004-FDESA-BS
PLURIN            SET SW-SI-LTA-FINAL TO TRUE
PLURIN         ELSE
PLURIN
PLURIN            IF WS-RENOVADA-LTA EQUAL 'N'
PLURIN               SET SW-SI-LTA-MITAD TO TRUE
PLURIN            END-IF
PLURIN         END-IF
RF001      ELSE
RF001          CONTINUE
RF001      END-IF

BONOA               PERFORM 5221-BUSCAR-TRAMO-ANTERIOR

           IF X350M004-CRETORNO = '00'
RF001 *****    MOVE X350M004-PPBBONO-BS  TO WS-PRREAPB-BS-ANTERIOR
RF001 *****    MOVE X350M004-PGASBONO-BS TO WS-PGASTOS-BS-ANTERIOR
RF001 *****    MOVE X350M004-KGRUPEMP-BS TO WS-KGRUPEMP-BS-ANTERIOR
RF001          MOVE X350M004-PPBBONO-BS-ANT TO WS-PRREAPB-BS-ANTERIOR
RF001          MOVE X350M004-PGASBONO-BS-ANT TO WS-PGASTOS-BS-ANTERIOR
RF001          MOVE X350M004-KGRUPEMP-BS-ANT TO WS-KGRUPEMP-BS-ANTERIOR
           END-IF

RF001*******        PERFORM 5222-BUSCAR-TRAMO-FUTURO
RF001********
RF001******         IF X350M004-CRETORNO = '00'
RF001*******          MOVE X350M004-PPBBONO-BS TO WS-PRREAPB-BS-FUTURO
RF001*******          MOVE X350M004-PGASBONO-BS TO WS-PGASTOS-BS-FUTURO
RF001********         MOVE X350M004-KGRUPEMP-BS TO WS-KGRUPEMP-BS-FUTURO
RF001******         END-IF
RF001 *******     IF ((X350M004-FDESA-BS EQUAL SPACES OR LOW-VALUES) AND
RF001 ********         X350M004-FHASA-BS-ANT > 2000) OR
      ********         X350M004-FDESA-BS < X350M004-FHASA-BS-ANT AND
      *********        SW-SI-LTA-SOLO-ANT-P
RF001 *******     IF ((X350M004-FDESA-BS EQUAL SPACES OR LOW-VALUES) AND
RF001 *********        X350M004-FHASA-BS-ANT > 2000) OR
RF001 **********      (X350M004-FDESA-BS < X350M004-FHASA-BS-ANT AND
RF001 ***********      SW-SI-LTA-SOLO-ANT-P) OR
RF001 ***********  (X350JXX2-KPOLIZA  NOT EQUAL X350M004-KPOLIZA-BS AND
RF001 ************  X350JXX2-KSUBPOL  NOT EQUAL X350M004-KSUBPOL-BS)
RF001 *D*
RF001 *    DISPLAY 'SW-NO-LTB ' SW-SW-LTB
RF001 *    DISPLAY 'SW-SW-LTBA' SW-SW-LTBA
RF001 *    DISPLAY 'SW-SW-LTP ' SW-SW-LTP
RF001 *    DISPLAY 'SW-SW-LTPA ' SW-SW-LTPA
RF001 *F*
RF001              IF X350M004-FDESA-BS > 0
RF001                  SET SW-SI-LTB TO TRUE

                    MOVE X350M004-FDESA-BS TO WS-ANNO-LTA-D
                    MOVE X350M004-FDESM-BS TO WS-MES-LTA-D
                    MOVE X350M004-FDESD-BS TO WS-DIA-LTA-D

                    MOVE X350M004-FHASA-BS TO WS-ANNO-LTA-H
                    MOVE X350M004-FHASM-BS TO WS-MES-LTA-H
                    MOVE X350M004-FHASD-BS TO WS-DIA-LTA-H
RF001              END-IF

               WHEN OTHER

                    MOVE '0000000'         TO X350M004-KPOLIZA-E
                    MOVE '00000'           TO X350M004-KSUBPOL-E

RF001****           CALL B350M004           USING WS-X350M004
RF001               CALL B350M004 USING WS-X350M006

                    EVALUATE X350M004-CRETORNO
                        WHEN '00'

210524     MOVE X350M004-PPBBONO-BS  TO WS-PRREAPB-BS-ACTUAL
210524     MOVE X350M004-PGASBONO-BS TO WS-PGASTOS-BS-ACTUAL
130624     MOVE X350M004-KGRUPEMP-BS TO WS-KGRUPEMP-BS-ACTUAL
PLURIN         MOVE X350M004-FDESA-BS TO    WS-FRENO-DESDE-ABONO-ACTUAL
PLURIN         MOVE X350M004-FDESM-BS TO    WS-FRENO-DESDE-MBONO-ACTUAL
PLURIN         MOVE X350M004-FDESD-BS TO    WS-FRENO-DESDE-DBONO-ACTUAL
RF001          MOVE X350M004-FHASA-BS TO    WS-FRENO-HASTA-ABONO-ACTUAL
RF001          MOVE X350M004-FHASM-BS TO    WS-FRENO-HASTA-MBONO-ACTUAL
RF001          MOVE X350M004-FHASD-BS TO    WS-FRENO-HASTA-DBONO-ACTUAL

RF001      IF X350M004-FDESA-BS > 0

PLURIN         IF WS-RENOVADA-LTA EQUAL 'N' AND
PLURIN            X350JXX2-FRENO-AHASTA = X350M004-FDESA-BS
PLURIN            SET SW-SI-LTA-FINAL TO TRUE
PLURIN         ELSE
PLURIN
PLURIN            IF WS-RENOVADA-LTA EQUAL 'N'
PLURIN               SET SW-SI-LTA-MITAD TO TRUE
PLURIN            END-IF
PLURIN         END-IF
RF001      ELSE
RF001          CONTINUE
RF001      END-IF

BONOA               PERFORM 5221-BUSCAR-TRAMO-ANTERIOR
RF001 *D*
RF001 *    DISPLAY 'SW-NO-LTB ' SW-SW-LTB
RF001 *    DISPLAY 'SW-SW-LTBA' SW-SW-LTBA
RF001 *    DISPLAY 'SW-SW-LTP ' SW-SW-LTP
RF001 *    DISPLAY 'SW-SW-LTPA ' SW-SW-LTPA
RF001 *F*

           IF X350M004-CRETORNO = '00'
RF001 *****    MOVE X350M004-PPBBONO-BS  TO WS-PRREAPB-BS-ANTERIOR
RF001 *****    MOVE X350M004-PGASBONO-BS TO WS-PGASTOS-BS-ANTERIOR
RF001 *****    MOVE X350M004-KGRUPEMP-BS TO WS-KGRUPEMP-BS-ANTERIOR
RF001          MOVE X350M004-PPBBONO-BS-ANT TO WS-PRREAPB-BS-ANTERIOR
RF001          MOVE X350M004-PGASBONO-BS-ANT TO WS-PGASTOS-BS-ANTERIOR
RF001          MOVE X350M004-KGRUPEMP-BS-ANT TO WS-KGRUPEMP-BS-ANTERIOR
           END-IF

RF001*******        PERFORM 5222-BUSCAR-TRAMO-FUTURO
RF001********
RF001******         IF X350M004-CRETORNO = '00'
RF001*******          MOVE X350M004-PPBBONO-BS TO WS-PRREAPB-BS-FUTURO
RF001*******          MOVE X350M004-PGASBONO-BS TO WS-PGASTOS-BS-FUTURO
RF001********         MOVE X350M004-KGRUPEMP-BS TO WS-KGRUPEMP-BS-FUTURO
RF001******         END-IF
RF001 *D*
RF001 *    DISPLAY 'X350M004-FDESA-BS ' X350M004-FDESA-BS
RF001 *    DISPLAY 'X350M004-FHASA-BS-ANT ' X350M004-FHASA-BS-ANT
RF001 *    DISPLAY 'X350M004-KPOLIZA-BS   ' X350M004-KPOLIZA-BS
RF001 *    DISPLAY 'X350M004-KSUBPOL-BS   ' X350M004-KSUBPOL-BS
RF001 *    DISPLAY 'X350M004-FHASA-BS-ANT ' X350M004-FHASA-BS-ANT
RF001 *    DISPLAY 'X350JXX2-KPOLIZA      ' X350JXX2-KPOLIZA
RF001 *    DISPLAY 'X350JXX2-KSUBPOL      ' X350JXX2-KSUBPOL
RF001 *F*
RF001 ********    IF ((X350M004-FDESA-BS EQUAL SPACES OR LOW-VALUES) AND
RF001 ********         X350M004-FHASA-BS-ANT > 2000) OR
      ********         X350M004-FDESA-BS < X350M004-FHASA-BS-ANT AND
      ********         SW-SI-LTA-SOLO-ANT-P
RF001 *******     IF ((X350M004-FDESA-BS EQUAL SPACES OR
      *********        X350M004-FDESA-BS EQUAL LOW-VALUES)
      **********      AND X350M004-FHASA-BS-ANT > 2000
      ***********     )
      *********** OR
      ************    (X350M004-FDESA-BS < X350M004-FHASA-BS-ANT
      *******         AND SW-SI-LTA-SOLO-ANT-P
      *********       )
      **********  OR
      ***********     (X350JXX2-KPOLIZA  NOT EQUAL X350M004-KPOLIZA-BS
      ***********     AND
RF001 ************     X350JXX2-KSUBPOL  NOT EQUAL X350M004-KSUBPOL-BS
      ************    )
RF001              IF X350M004-FDESA-BS > 0
RF001                  SET SW-SI-LTB TO TRUE

                        MOVE X350M004-FDESA-BS TO WS-ANNO-LTA-D
                        MOVE X350M004-FDESM-BS TO WS-MES-LTA-D
                        MOVE X350M004-FDESD-BS TO WS-DIA-LTA-D

                        MOVE X350M004-FHASA-BS TO WS-ANNO-LTA-H
                        MOVE X350M004-FHASM-BS TO WS-MES-LTA-H
                        MOVE X350M004-FHASD-BS TO WS-DIA-LTA-H
RF001              END-IF

                   WHEN OTHER
      *D*
      *    DISPLAY 'NO LTB'
      *F*
                    SET SW-NO-LTB TO TRUE
                   END-EVALUATE
           END-EVALUATE
           .

      ******************************************************************10650099
BONOA  5214-LEER-ENTRADA-ANTERIOR.                                      10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5214-LEER-ENTRADA-ANTERIOR'
      *F*
           INITIALIZE R350T077
      *
           READ ENTRADA-ANTERIOR INTO R350T077                          10740099
             AT END                                                     10750099
                SET SW-SI-FINAL-BONO-ANT TO TRUE                        10760099
                SET SW-SI-FINAL-PLUR-ANT TO TRUE                        10760099
                MOVE HIGH-VALUES TO WS-CLAVE1-LTA
             NOT AT END                                                 10770099
      *D*
      *    DISPLAY 'R350T077-KPOLIZA(4:7) ' R350T077-KPOLIZA(4:7)
      *    DISPLAY 'R350T077-KSUBPOLIZA   ' R350T077-KSUBPOLIZA
      *F*
                MOVE R350T077-KPOLIZA(4:7) TO WS-POLIZA-1-LTA
                MOVE R350T077-KSUBPOLIZA   TO WS-SUBPOL-1-LTA

           END-READ.                                                    10880099
      *                                                                 10890099
      ******************************************************************
BONOA  5221-BUSCAR-TRAMO-ANTERIOR.
      ******************************************************************
      *D*
      *    DISPLAY '5221-BUSCAR-TRAMO-ANTERIOR'
      *F*
           IF X350M004-CTIPO-E = 'P'
              MOVE X350M004-FDESA-PS TO WS-ANNO-DESDE-TRAMO
                                        X350M004-FDESA-E
              MOVE X350M004-FDESM-PS TO WS-MES-DESDE-TRAMO
                                        X350M004-FDESM-E
              MOVE X350M004-FDESD-PS TO WS-DIA-DESDE-TRAMO
                                        X350M004-FDESD-E

              MOVE X350M004-FHASA-PS TO WS-ANNO-HASTA-TRAMO
              MOVE X350M004-FHASM-PS TO WS-MES-HASTA-TRAMO
              MOVE X350M004-FHASD-PS TO WS-DIA-HASTA-TRAMO

              SET SW-NO-LTPA         TO TRUE
           END-IF

           IF X350M004-CTIPO-E = 'B'
              MOVE X350M004-FDESA-BS TO WS-ANNO-DESDE-TRAMO
                                        X350M004-FDESA-E
              MOVE X350M004-FDESM-BS TO WS-MES-DESDE-TRAMO
                                        X350M004-FDESM-E
              MOVE X350M004-FDESD-BS TO WS-DIA-DESDE-TRAMO
                                        X350M004-FDESD-E

              MOVE X350M004-FHASA-BS TO WS-ANNO-HASTA-TRAMO
              MOVE X350M004-FHASM-BS TO WS-MES-HASTA-TRAMO
              MOVE X350M004-FHASD-BS TO WS-DIA-HASTA-TRAMO

              SET SW-NO-LTBA         TO TRUE
           END-IF

RF001**    IF X350M004-FDESD-E = 1
RF001**       MOVE 28 TO X350M004-FDESD-E
RF001**
RF001**       IF X350M004-FDESM-E = 1
RF001**          MOVE 12 TO X350M004-FDESM-E
RF001**
RF001**          SUBTRACT 1 FROM X350M004-FDESA-E
RF001**       ELSE
RF001**          SUBTRACT 1 FROM X350M004-FDESM-E
RF001**       END-IF
RF001**    ELSE
RF001**       SUBTRACT 1 FROM X350M004-FDESD-E
RF001**    END-IF
RF001**
RF001**    MOVE X350M004-FDESA-E TO X350M004-FHASA-E
RF001**    MOVE X350M004-FDESM-E TO X350M004-FHASM-E
RF001**    MOVE X350M004-FDESD-E TO X350M004-FHASD-E
RF001**
RF0001**   CALL B350M004 USING WS-X350M004

      *D*
      *    DISPLAY 'X350M004-CTIPO-E ' X350M004-CTIPO-E
      *    DISPLAY 'X350M004-PBEMIPLU-PS-ANT ' X350M004-PBEMIPLU-PS-ANT
      *    DISPLAY 'X350M004-FDESA-PS-ANT    ' X350M004-FDESA-PS-ANT
      *F*
RF001***** IF X350M004-CRETORNO = '00'
              IF (X350M004-CTIPO-E = 'P') AND
RF001*****       (X350M004-PBEMIPLU-PS = 0)
RF001            (X350M004-PBEMIPLU-PS-ANT = 0) AND
RF001            (X350M004-FDESA-PS-ANT > 0)
      *D*
      *    DISPLAY 'ENTRO -IF- ANTERIOR-PLURI '
      *F*

RF001 ***        MOVE X350M004-FDESA-PS TO WS-ANNO-DESDE-TRAMO-ANT
RF001 ***        MOVE X350M004-FDESM-PS TO WS-MES-DESDE-TRAMO-ANT
RF001 ***        MOVE X350M004-FDESD-PS TO WS-DIA-DESDE-TRAMO-ANT
RF001
RF001 ***        MOVE X350M004-FHASA-PS TO WS-ANNO-HASTA-TRAMO-ANT
RF001 ***        MOVE X350M004-FHASM-PS TO WS-MES-HASTA-TRAMO-ANT
RF001 ***        MOVE X350M004-FHASD-PS TO WS-DIA-HASTA-TRAMO-ANT
RF001
RF001            MOVE X350M004-FDESA-PS-ANT
RF001              TO WS-ANNO-DESDE-TRAMO-ANT
RF001            MOVE X350M004-FDESM-PS-ANT
RF001              TO WS-MES-DESDE-TRAMO-ANT
RF001            MOVE X350M004-FDESD-PS-ANT
RF001              TO WS-DIA-DESDE-TRAMO-ANT
RF001
RF001            MOVE X350M004-FHASA-PS-ANT
RF001              TO WS-ANNO-HASTA-TRAMO-ANT
RF001            MOVE X350M004-FHASM-PS-ANT
RF001              TO WS-MES-HASTA-TRAMO-ANT
RF001            MOVE X350M004-FHASD-PS-ANT
RF001              TO WS-DIA-HASTA-TRAMO-ANT
                 SET SW-SI-LTPA         TO TRUE
RF001         END-IF

              IF (X350M004-CTIPO-E = 'B') AND
RF001******      (X350M004-PBBONOEMI-BS = 0)
RF001            (X350M004-PBBONOEMI-BS-ANT = 0) AND
                 (X350M004-FDESA-BS-ANT > 0)
      *D*
      *    DISPLAY 'ENTRO -IF- ANTERIOR-BONO '
      *F*

RF001 ***        MOVE X350M004-FDESA-BS TO WS-ANNO-DESDE-TRAMO-ANT
RF001 ***        MOVE X350M004-FDESM-BS TO WS-MES-DESDE-TRAMO-ANT
RF001 ***        MOVE X350M004-FDESD-BS TO WS-DIA-DESDE-TRAMO-ANT
RF001
RF001 ***        MOVE X350M004-FHASA-BS TO WS-ANNO-HASTA-TRAMO-ANT
RF001 ***        MOVE X350M004-FHASM-BS TO WS-MES-HASTA-TRAMO-ANT
RF001 ***        MOVE X350M004-FHASD-BS TO WS-DIA-HASTA-TRAMO-ANT
RF001
RF001            MOVE X350M004-FDESA-BS-ANT
RF001              TO WS-ANNO-DESDE-TRAMO-ANT
RF001            MOVE X350M004-FDESM-BS-ANT
RF001              TO WS-MES-DESDE-TRAMO-ANT
RF001            MOVE X350M004-FDESD-BS-ANT
RF001              TO WS-DIA-DESDE-TRAMO-ANT
RF001
RF001            MOVE X350M004-FHASA-BS-ANT
RF001              TO WS-ANNO-HASTA-TRAMO-ANT
RF001            MOVE X350M004-FHASM-BS-ANT
RF001              TO WS-MES-HASTA-TRAMO-ANT
RF001            MOVE X350M004-FHASD-BS-ANT
RF001              TO WS-DIA-HASTA-TRAMO-ANT
                 SET SW-SI-LTBA         TO TRUE
              END-IF
RF001****  END-IF

           IF WS-FECHA-DESDE-TRAMO-ANT NOT > '00000000'
              IF X350M004-CTIPO-E = 'P'
                 MOVE WS-ANNO-DESDE-TRAMO TO X350M004-FDESA-PS
                 MOVE WS-MES-DESDE-TRAMO  TO X350M004-FDESM-PS
                 MOVE WS-DIA-DESDE-TRAMO  TO X350M004-FDESD-PS
              END-IF

              IF X350M004-CTIPO-E = 'B'
                 MOVE WS-ANNO-DESDE-TRAMO TO X350M004-FDESA-BS
                 MOVE WS-MES-DESDE-TRAMO  TO X350M004-FDESM-BS
                 MOVE WS-DIA-DESDE-TRAMO  TO X350M004-FDESD-BS
              END-IF
           END-IF

           IF X350M004-CTIPO-E = 'P'
              MOVE WS-ANNO-HASTA-TRAMO TO X350M004-FHASA-PS
              MOVE WS-MES-HASTA-TRAMO  TO X350M004-FHASM-PS
              MOVE WS-DIA-HASTA-TRAMO  TO X350M004-FHASD-PS
           END-IF

           IF X350M004-CTIPO-E = 'B'
              MOVE WS-ANNO-HASTA-TRAMO TO X350M004-FHASA-BS
              MOVE WS-MES-HASTA-TRAMO  TO X350M004-FHASM-BS
              MOVE WS-DIA-HASTA-TRAMO  TO X350M004-FHASD-BS
           END-IF

           MOVE '00' TO X350M004-CRETORNO
           .
BONOA
      ******************************************************************10650099
LTA    5211-GESTION-MOVIMIENTOS-LTB.                                    10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5211-GESTION-MOVIMIENTOS-LTB'
      *    DISPLAY 'WS-CLAVE1-LTA     ' WS-CLAVE1-LTA
      *    DISPLAY 'WS-CLAVE1-ANT-LTA ' WS-CLAVE1-ANT-LTA
      *    DISPLAY 'X350JPX3-KSG-POLSUB ' X350JPX3-KSG-POLSUB
      *F*
           MOVE WS-CLAVE1-LTA    TO WS-CLAVE1-ANT-LTA
           MOVE WS-IMPORTE-PBP   TO WS-IMPORTE-PBP-AUX

           IF WS-CLAVE1-ANT-LTA NOT EQUAL X350JPX3-KSG-POLSUB
              MOVE X350JPX3-KSG-POLSUB TO WS-CLAVE1-ANT-LTA
              MOVE X350JPX3-KSG-POLSUB TO WS-CLAVE1-LTA
           END-IF
           INITIALIZE WS-IMPORTE-205
                      WS-IMPORTE-205-NR
                      WS-IMPORTE-206
                      WS-IMPORTE-206-SINI
                      WS-IMPORTE-902
                      WS-IMPORTE-902-NR
                      WS-IMPORTE-206-NR
                      WS-IMP-PERI-CAL1
                      WS-IMP-PERI-CAL2
                      WS-IMP-PERI-CAL3
                      WS-IMP-PERI-CALC
                      WS-IMPORTE-PBP
      *               WS-AUXILIAR

      *    PERFORM 5213-LEER-ENTRADA-MOVS-LTA

      *D*
      *    DISPLAY 'WS-CLAVE1-LTA     ' WS-CLAVE1-LTA
      *    DISPLAY 'WS-CLAVE1-ANT-LTA ' WS-CLAVE1-ANT-LTA
      *F*
           PERFORM 5212-ACUMULAR-MOVS-LTB
             UNTIL WS-CLAVE1-LTA NOT EQUAL WS-CLAVE1-ANT-LTA OR
                   SW-SI-FINAL-MOVS-LTA

           ADD WS-IMPORTE-PBP-AUX TO WS-IMPORTE-PBP
      *    PERFORM 5214-CALCULOS-LTB
           .

      ******************************************************************10650099
BONOA  5211-GESTION-MOVIMIENTOS-ANT.                                    10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5211-GESTION-MOVIMIENTOS-ANT'
      *    DISPLAY 'X350JPX3-KSG-POLSUB ' X350JPX3-KSG-POLSUB
      *    DISPLAY 'WS-CLAVE1-LTA ' WS-CLAVE1-LTA
      *F*
      *    MOVE WS-CLAVE1-LTA    TO WS-CLAVE1-ANT-LTA

           MOVE ZEROES TO X350JPX5-KSG-POLSUB

           INITIALIZE WS-IMPORTE-205
                      WS-IMPORTE-205-NR
                      WS-IMPORTE-206
                      WS-IMPORTE-206-SINI
                      WS-IMPORTE-902
                      WS-IMPORTE-902-NR
                      WS-IMPORTE-206-NR
                      WS-IMP-PERI-CAL1
                      WS-IMP-PERI-CAL2
                      WS-IMP-PERI-CAL3
                      WS-IMP-PERI-CALC
                      WS-IMPORTE-PBP-ANT
      *               WS-AUXILIAR
           OPEN INPUT ENTRADA-ANTERIOR

           PERFORM 5214-LEER-ENTRADA-ANTERIOR

           PERFORM 5212-ACUMULAR-MOVS-ANTERIOR
             UNTIL X350JPX3-KSG-POLSUB < WS-CLAVE1-LTA OR
                   SW-SI-FINAL-BONO-ANT OR SW-SI-FINAL-PLUR-ANT

           CLOSE  ENTRADA-ANTERIOR

           SET SW-NO-FINAL-BONO-ANT TO TRUE
           SET SW-NO-FINAL-PLUR-ANT TO TRUE
      *D*
      *    DISPLAY 'SALIDA ACUMULAR ANT ' WS-IMPORTE-205
      *F*
      *    PERFORM 5214-CALCULOS-LTB
           .

      ******************************************************************10650099
LTA    5212-ACUMULAR-MOVS-LTB.                                          10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5212-ACUMULAR-MOVS-LTB'
      *    DISPLAY 'X350JXX2-KPOLIZA      ' X350JXX2-KPOLIZA
      *    DISPLAY 'X350JXX2-KSUBPOL      ' X350JXX2-KSUBPOL
      *    DISPLAY 'X350JPX3-KPOLIZA      ' X350JPX3-KPOLIZA
      *    DISPLAY 'X350JPX3-KSUBPOL      ' X350JPX3-KSUBPOL
      *    DISPLAY 'R350T077-KMOVIMIENTO  ' R350T077-KMOVIMIENTO
      *    DISPLAY 'R350T077-FCONTABLE    ' R350T077-FCONTABLE
      *    DISPLAY 'R350T077-FEFECTO      ' R350T077-FEFECTO
      *    DISPLAY 'WS-FECHA-DESDE-TRAMO  ' WS-FECHA-DESDE-TRAMO
      *    DISPLAY 'WS-FECHA-HASTA-TRAMO  ' WS-FECHA-HASTA-TRAMO
      *    DISPLAY 'R350T000-FFINAL       ' R350T000-FFINAL
      *F*
151124*         IF X350JXX2-FRENO-HASTA   >  R350T000-FFINAL
131125          IF X350JPX3-FRENO-HASTA   >  R350T000-FFINAL
151124             MOVE 'S'                   TO WS-RENOVADA-ALFA
151124          ELSE
151124             MOVE 'N'                   TO WS-RENOVADA-ALFA
151124          END-IF

151124          IF WS-RENOVADA-ALFA = 'S'
151124*            MOVE X350JXX2-FRENO-DESDE TO WS-FECHA-ALFA
131225             MOVE X350JPX3-FRENO-DESDE TO WS-FECHA-ALFA
151124          ELSE
151124*            MOVE X350JXX2-FRENO-DESDE TO WS-FECHA-ALFA
131225             MOVE X350JPX3-FRENO-DESDE TO WS-FECHA-ALFA
151124             ADD 1 TO WS-ANNO-FECHA-ALFA
151124          END-IF

LUNA  *    MOVE WS-FECHA-DESDE-TRAMO TO
LUNA  *                                 WS-FECSIN-DESDE
PLURIN     MOVE WS-FRENO-DESDE-ABONO-ACTUAL TO
PLURIN                                  WS-FECSIN-DESDE
LUNA       MOVE WS-FECHA-HASTA-TRAMO TO
LUNA                                    WS-FECSIN-HASTA

           IF R350T077-IMPORTE IS NUMERIC

              EVALUATE R350T077-KCONCEPTO

                  WHEN '0902'
                           ADD R350T077-IMPORTE TO WS-IMPORTE-902
              END-EVALUATE
           ELSE
              EVALUATE R350T077-KCONCEPTO
                  WHEN '0205'
      *D*
      *       DISPLAY 'ACUMULAR 205'
      *       DISPLAY 'R350T077-IMPORTE-D ' R350T077-IMPORTE-D
      *F*
240524*               IF R350T077-FCONTABLE >= X350JPX3-FRENO-DESDE
110624*               IF R350T077-FCONTABLE >= WS-FECHA-HASTA-TRAMO
110624                IF R350T077-FCONTABLE >= WS-FECHA-ALFA
      *              AND R350T077-FCONTABLE <  X350JPX3-FRENO-HASTA
JOSE  *               IF R350T077-FCONTABLE >= WS-FECHA-DESDE-TRAMO
JOSE  *              AND R350T077-FCONTABLE <  WS-FECHA-HASTA-TRAMO

                       ADD R350T077-IMPORTE-D TO WS-IMPORTE-205

RF003                     IF  SW-SI-LTBA
                              MOVE WS-FECHA-DESDE-TRAMO-ANT TO
                              WS-FRENO-DESDE-BONOA-PBPAG
                              MOVE WS-FECHA-HASTA-TRAMO-ANT TO
                              WS-FRENO-HASTA-BONOA-PBPAG

RF003                         COMPUTE WS-FRENO-DESDE-BONOA-PBPAG =
RF003                                 WS-FRENO-DESDE-BONOA-PBPAG + 10000
RF003                         COMPUTE WS-FRENO-HASTA-BONOA-PBPAG =
RF003                                 WS-FRENO-HASTA-BONOA-PBPAG + 10000
RF003                     END-IF
RF003                     IF SW-SI-LTB
                              MOVE WS-FRENO-DESDE-BONO-ACTUAL TO
                                   WS-FRENO-DESDE-BONO-PBPAG
                              MOVE WS-FRENO-HASTA-BONO-ACTUAL TO
                                   WS-FRENO-HASTA-BONO-PBPAG
RF003                         COMPUTE WS-FRENO-DESDE-BONO-PBPAG =
RF003                                 WS-FRENO-DESDE-BONO-PBPAG  + 10000
RF003                         COMPUTE WS-FRENO-HASTA-BONO-PBPAG =
RF003                                 WS-FRENO-HASTA-BONO-PBPAG  + 10000
RF003                     END-IF
RF003               IF ( SW-SI-LTBA
RF003               AND R350T077-FCONTABLE >= WS-FRENO-DESDE-BONOA-PBPAG
RF003              AND R350T077-FCONTABLE <  WS-FRENO-HASTA-BONOA-PBPAG)
RF003                    ADD R350T077-PBPAGADA  TO
RF003                                          WS-IMPORTE-PBP-BONO-FASE2
RF003               END-IF
RF003               IF ( SW-SI-LTB
RF003               AND R350T077-FCONTABLE >= WS-FRENO-DESDE-BONO-PBPAG
RF003              AND R350T077-FCONTABLE <  WS-FRENO-HASTA-BONO-PBPAG)
RF003                    ADD R350T077-PBPAGADA  TO WS-IMPORTE-PBP
RF003               END-IF

300524*                ADD R350T077-IMPORTE-D TO WS-IMP-PERI-CAL1
300524                 ADD R350T077-IMPORTE-D TO WS-IMP-PERI-CAL3
      *D*
      *       DISPLAY 'WS-IMP-PERI-CAL3-BONO ' WS-IMP-PERI-CAL3
      *F*
                      ELSE
      *                 IF R350T077-FCONTABLE < X350JPX3-FRENO-DESDE
                          ADD R350T077-IMPORTE-D TO WS-IMPORTE-205
RF003                     IF  SW-SI-LTBA
                              MOVE WS-FECHA-DESDE-TRAMO-ANT TO
                              WS-FRENO-DESDE-BONOA-PBPAG
                              MOVE WS-FECHA-HASTA-TRAMO-ANT TO
                              WS-FRENO-HASTA-BONOA-PBPAG

RF003                         COMPUTE WS-FRENO-DESDE-BONOA-PBPAG =
RF003                                 WS-FRENO-DESDE-BONOA-PBPAG + 10000
RF003                         COMPUTE WS-FRENO-HASTA-BONOA-PBPAG =
RF003                                 WS-FRENO-HASTA-BONOA-PBPAG + 10000
RF003                     END-IF
RF003                     IF SW-SI-LTB
                              MOVE WS-FRENO-DESDE-BONO-ACTUAL TO
                                   WS-FRENO-DESDE-BONO-PBPAG
                              MOVE WS-FRENO-HASTA-BONO-ACTUAL TO
                                   WS-FRENO-HASTA-BONO-PBPAG
RF003                         COMPUTE WS-FRENO-DESDE-BONO-PBPAG =
RF003                                 WS-FRENO-DESDE-BONO-PBPAG  + 10000
RF003                         COMPUTE WS-FRENO-HASTA-BONO-PBPAG =
RF003                                 WS-FRENO-HASTA-BONO-PBPAG  + 10000
RF003                     END-IF
RF003               IF ( SW-SI-LTBA
RF003               AND R350T077-FCONTABLE >= WS-FRENO-DESDE-BONOA-PBPAG
RF003              AND R350T077-FCONTABLE <  WS-FRENO-HASTA-BONOA-PBPAG)
RF003                    ADD R350T077-PBPAGADA  TO
RF003                                          WS-IMPORTE-PBP-BONO-FASE2
RF003               END-IF
RF003               IF ( SW-SI-LTB
RF003               AND R350T077-FCONTABLE >= WS-FRENO-DESDE-BONO-PBPAG
RF003              AND R350T077-FCONTABLE <  WS-FRENO-HASTA-BONO-PBPAG)
RF003                    ADD R350T077-PBPAGADA  TO WS-IMPORTE-PBP
RF003               END-IF

                          ADD R350T077-IMPORTE-D TO WS-IMP-PERI-CAL2
      *D*
      *       DISPLAY 'WS-IMP-PERI-CAL2-BONO ' WS-IMP-PERI-CAL2
      *F*
                        END-IF

JOSE  *                 IF R350T077-FCONTABLE >= X350JPX3-FRENO-HASTA
JOSE  *                AND R350T077-FCONTABLE <= R350T000-FFINAL
JOSE  *                 IF R350T077-FEFECTO   >  R350T000-FFINAL
JOSE  *                AND R350T077-FCONTABLE <= R350T000-FFINAL
      *                   ADD R350T077-IMPORTE-D TO WS-IMPORTE-205
      *                   ADD R350T077-PBPAGADA  TO WS-IMPORTE-PBP
      *                   ADD R350T077-IMPORTE-D TO WS-IMP-PERI-CAL3
      *                 END-IF
      *               END-IF

                  WHEN '0206'
                        IF R350T077-FCONTABLE < WS-FECHA-HASTA-TRAMO OR
                           R350T077-FEFECTO < WS-FECHA-HASTA-TRAMO
PLURIN*                    IF R350T077-FCONTABLE > WS-FECHA-DESDE-TRAMO
PLURIN*                    AND R350T077-FEFECTO  >  WS-FECHA-DESDE-TRAMO
                           IF R350T077-FCONTABLE >
PLURIN                                        WS-FRENO-DESDE-BONO-ACTUAL
                           AND R350T077-FEFECTO  >
PLURIN                                        WS-FRENO-DESDE-BONO-ACTUAL
                           AND SW-SI-LTBA
                              ADD R350T077-IMPORTE-D TO WS-IMPORTE-206
      *D*
      *       DISPLAY ' WS-IMPORTE-206-IF ' WS-IMPORTE-206
      *F*
                           ELSE
      *D*
      *       DISPLAY ' WS-IMPORTE-206-ELSE' WS-IMPORTE-206-SINI
      *F*
2404                       IF R350T077-FCARGA    >
2404                                      WS-FRENO-DESDE-BONO-ACTUAL OR
2404
2404                         ( R350T077-CESTADORE = '01' OR
2404                           R350T077-CESTADORE = '02' OR
2404                           R350T077-CESTADORE = '04' OR
2404                           R350T077-CESTADORE = '05')
                           ADD R350T077-IMPORTE-D TO WS-IMPORTE-206-SINI
                           END-IF
                        ELSE
                          ADD R350T077-IMPORTE-D TO WS-IMPORTE-206-NR

                        END-IF
                  WHEN '0902'
                        ADD R350T077-IMPORTE-D TO WS-IMPORTE-902
                        ADD R350T077-IMPORTE-D TO WS-IMP-PERI-CAL3
RF003                     IF  SW-SI-LTBA
                              MOVE WS-FECHA-DESDE-TRAMO-ANT TO
                              WS-FRENO-DESDE-BONOA-PBPAG
                              MOVE WS-FECHA-HASTA-TRAMO-ANT TO
                              WS-FRENO-HASTA-BONOA-PBPAG

RF003                         COMPUTE WS-FRENO-DESDE-BONOA-PBPAG =
RF003                                 WS-FRENO-DESDE-BONOA-PBPAG + 10000
RF003                         COMPUTE WS-FRENO-HASTA-BONOA-PBPAG =
RF003                                 WS-FRENO-HASTA-BONOA-PBPAG + 10000
RF003                     END-IF
RF003                     IF SW-SI-LTB
                              MOVE WS-FRENO-DESDE-BONO-ACTUAL TO
                                   WS-FRENO-DESDE-BONO-PBPAG
                              MOVE WS-FRENO-HASTA-BONO-ACTUAL TO
                                   WS-FRENO-HASTA-BONO-PBPAG
RF003                         COMPUTE WS-FRENO-DESDE-BONO-PBPAG =
RF003                                 WS-FRENO-DESDE-BONO-PBPAG  + 10000
RF003                         COMPUTE WS-FRENO-HASTA-BONO-PBPAG =
RF003                                 WS-FRENO-HASTA-BONO-PBPAG  + 10000
RF003                     END-IF
RF003               IF ( SW-SI-LTBA
RF003               AND R350T077-FCONTABLE >= WS-FRENO-DESDE-BONOA-PBPAG
RF003              AND R350T077-FCONTABLE <  WS-FRENO-HASTA-BONOA-PBPAG)
RF003                    ADD R350T077-PBPAGADA  TO
RF003                                          WS-IMPORTE-PBP-BONO-FASE2
RF003               END-IF
RF003               IF ( SW-SI-LTB
RF003               AND R350T077-FCONTABLE >= WS-FRENO-DESDE-BONO-PBPAG
RF003              AND R350T077-FCONTABLE <  WS-FRENO-HASTA-BONO-PBPAG)
RF003                    ADD R350T077-PBPAGADA  TO WS-IMPORTE-PBP
RF003               END-IF
      *D*
      *       DISPLAY 'WS-IMP-PERI-CAL3-BONO-902 ' WS-IMP-PERI-CAL3
      *F*
              END-EVALUATE
           END-IF

           MOVE WS-POLIZA-1-LTA            TO WS-POLIZA-1-ANT-LTA
           MOVE WS-SUBPOL-1-LTA            TO WS-SUBPOL-1-ANT-LTA

      *D*
      *    DISPLAY 'WS-CLAVE1-ANT-LTA ' WS-CLAVE1-ANT-LTA
      *F*
           PERFORM 5213-LEER-ENTRADA-MOVS-LTA

           .

      ******************************************************************10650099
BONOA  5212-ACUMULAR-MOVS-ANTERIOR.                                     10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5212-ACUMULAR-MOVS-ANTERIOR'
      *    DISPLAY 'X350JPX3-KSG-POLSUB        '  X350JPX3-KSG-POLSUB
      *    DISPLAY 'WS-CLAVE1-LTA              '  WS-CLAVE1-LTA
      *F*
LUNA       MOVE WS-FECHA-DESDE-TRAMO-ANT TO
LUNA                                    WS-FECSIN-DESDE
LUNA       MOVE WS-FECHA-HASTA-TRAMO-ANT TO
LUNA                                    WS-FECSIN-HASTA
           IF X350JPX3-KSG-POLSUB = WS-CLAVE1-LTA
              IF R350T077-IMPORTE IS NUMERIC

                 EVALUATE R350T077-KCONCEPTO

                     WHEN '0902'
                          ADD R350T077-IMPORTE TO WS-IMPORTE-902
                 END-EVALUATE
              ELSE
                 EVALUATE R350T077-KCONCEPTO
                     WHEN '0205'
RF001         IF WS-FECHA-LTA-D EQUAL ZEROS
RF001            MOVE WS-FECHA-DESDE-TRAMO-ANT TO WS-FECHA-LTA-D
RF001         END-IF

              IF R350T077-FCONTABLE >= WS-FECHA-DESDE-TRAMO-ANT AND
                 R350T077-FCONTABLE <= WS-FECHA-HASTA-TRAMO-ANT

                       ADD R350T077-IMPORTE-D TO WS-IMPORTE-205

RF003                     IF  SW-SI-LTBA
                              MOVE WS-FECHA-DESDE-TRAMO-ANT TO
                              WS-FRENO-DESDE-BONOA-PBPAG
                              MOVE WS-FECHA-HASTA-TRAMO-ANT TO
                              WS-FRENO-HASTA-BONOA-PBPAG

RF003                         COMPUTE WS-FRENO-DESDE-BONOA-PBPAG =
RF003                                 WS-FRENO-DESDE-BONOA-PBPAG + 10000
RF003                         COMPUTE WS-FRENO-HASTA-BONOA-PBPAG =
RF003                                 WS-FRENO-HASTA-BONOA-PBPAG + 10000
RF003                     END-IF
RF003               IF ( SW-SI-LTBA
RF003               AND R350T077-FCONTABLE >= WS-FRENO-DESDE-BONOA-PBPAG
RF003              AND R350T077-FCONTABLE <  WS-FRENO-HASTA-BONOA-PBPAG)
                          ADD R350T077-PBPAGADA  TO WS-IMPORTE-PBP-ANT
      *                   ADD R350T077-PBPAGADA  TO
      *                                     WS-IMPORTE-PBP-ANT-BONO
RF003               END-IF
      *                ADD R350T077-IMPORTE-D TO WS-IMP-PERI-CAL1
              END-IF

                  WHEN '0206'
      *                 IF R350T077-FCONTABLE < WS-FECHA-LTA-H    OR
      *                    R350T077-FEFECTO < WS-FECHA-LTA-H

      *D*
      *        DISPLAY 'ES-RENOVADA: ' WS-RENOVADA
      *        DISPLAY 'HASTA-TRAMO-ANT: ' WS-FECHA-HASTA-TRAMO-ANT
      *F*
LUNN  *             IF WS-RENOVADA = 'N' AND WS-ANTPBAN = 'S'
LUNN  *               MOVE WS-FECSIN-DESDE-PB TO WS-FECSIN-DESDE
      *             ELSE
      *               MOVE WS-FECHA-DESDE-TRAMO-ANT TO WS-FECSIN-DESDE
LUNN  *             END-IF
RF001          IF SW-NO-LTP AND SW-SI-LTPA
      *D*
      *       DISPLAY ' ME VOY ELSE NUEVO '
      *       DISPLAY ' X350JXX2-FRENO-DESDE ' X350JXX2-FRENO-DESDE
      *
      *F*

RF001               IF R350T077-FEFECTO   < WS-FECHA-HASTA-TRAMO-ANT
RF001 *                R350T077-FEFECTO    <= X350JXX2-FRENO-DESDE
JOSE27*                IF R350T077-FEFECTO   > WS-FECHA-DESDE-TRAMO-ANT
JOSE27*                AND R350T077-FEFECTO   < WS-FECHA-HASTA-TRAMO-ANT
                          ADD R350T077-IMPORTE-D TO WS-IMPORTE-206


JOSE27              END-IF
RF001          ELSE
                    IF R350T077-FCONTABLE < WS-FECHA-HASTA-TRAMO-ANT OR
                       R350T077-FEFECTO < WS-FECHA-HASTA-TRAMO-ANT
JOSE27*                IF R350T077-FEFECTO   > WS-FECHA-DESDE-TRAMO-ANT
JOSE27*                AND R350T077-FEFECTO   < WS-FECHA-HASTA-TRAMO-ANT
                          ADD R350T077-IMPORTE-D TO WS-IMPORTE-206
      *D*
      *       DISPLAY 'R350T077-KMOVIMIENTO  ' R350T077-KMOVIMIENTO
      *       DISPLAY ' WS-IMPORTE-206 ' WS-IMPORTE-206
      *F*
JOSE27*                 END-IF
JOSE27*                 ELSE
JOSE27*                   ADD R350T077-IMPORTE-D TO WS-IMPORTE-206-NR

                    END-IF
JOSE27         END-IF

                  WHEN '0902'
                        ADD R350T077-IMPORTE-D TO WS-IMPORTE-902
                 END-EVALUATE
              END-IF
           END-IF

           MOVE WS-POLIZA-1-LTA            TO WS-POLIZA-1-ANT-LTA
           MOVE WS-SUBPOL-1-LTA            TO WS-SUBPOL-1-ANT-LTA

           PERFORM 5214-LEER-ENTRADA-ANTERIOR

           .

      ******************************************************************10650099
LTA    5213-LEER-ENTRADA-MOVS-LTA.                                      10660099
      ******************************************************************10670099
           INITIALIZE R350T077
      *
           READ ENTRADA-MOVS-LTA INTO R350T077                          10740099
             AT END                                                     10750099
                SET SW-SI-FINAL-MOVS-LTA TO TRUE                        10760099
                MOVE HIGH-VALUES TO WS-CLAVE1-LTA
             NOT AT END                                                 10770099

FECHAS          IF (R350T077-DATOS-CUENTA(42:8) > R350T000-FFINAL AND
FECHAS             R350T077-KCONCEPTO = '0205') OR
FECHAS             R350T077-CTIPOREGMOV (1:2) = 'AP'
                   MOVE '0902' TO R350T077-KCONCEPTO
      *D*
      *    DISPLAY 'ENTRO Y CAMBIO 0205 POR 902'
      *F*
                END-IF
                MOVE R350T077-KPOLIZA(4:7) TO WS-POLIZA-1-LTA
                MOVE R350T077-KSUBPOLIZA   TO WS-SUBPOL-1-LTA

           END-READ.                                                    10880099
      *                                                                 10890099
      ******************************************************************10650099
LTA    5214-CALCULOS-LTP.                                               10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5214-CALCULOS-LTP'
      *F*
           COMPUTE AUX-PGASTOS-S = (100 - AUX-PGASTOS-S) / 100
           COMPUTE AUX-PCALCULOPB-S = AUX-PCALCULOPB-S / 100
           COMPUTE AUX-SINIESTROS-LTP = AUX-SINIESTROS-LTP +
                                        WS-IMPORTE-206
           IF X350JPX4-EPNETA NOT NUMERIC
              MOVE ZEROES TO X350JPX4-EPNETA
           END-IF

           COMPUTE AUX-REAJUSTE-LTP =
           (AUX-PCALCULOPB-S * (AUX-PGASTOS-S * X350JPX4-EPNETA -
            AUX-SINIESTROS-LTP))

           IF AUX-REAJUSTE-LTP < ZEROES
              MOVE ZEROES TO AUX-REAJUSTE-LTP
           ELSE
              MOVE AUX-REAJUSTE-LTP TO RSAL-PB-TECNICA
           END-IF
           .
      ******************************************************************10650099
LTA    5214-CALCULOS-LTB.                                               10660099
      ******************************************************************10670099
      *D*
      *    DISPLAY '5214-CALCULOS-LTB'
      *F*
           COMPUTE AUX-PGASTOS-S = (100 - AUX-PGASTOS-S) / 100
           COMPUTE AUX-PCALCULOPB-S = AUX-PCALCULOPB-S / 100
           COMPUTE AUX-SINIESTROS-LTB = AUX-SINIESTROS-LTB
                                      + WS-IMPORTE-206

           IF X350JPX4-EPNETA NOT NUMERIC
              MOVE ZEROES TO X350JPX4-EPNETA
           END-IF

           INITIALIZE RSAL-PB-TECNICA
      *               RSAL-DEV-NETCOASE

           COMPUTE AUX-REAJUSTE-LTB =
           (AUX-PGASTOS-S * (X350JPX5-PRIMA-PERIOD -
            WS-IMPORTE-206))

           MOVE WS-IMPORTE-PBP         TO RSAL-PBPAGA

           COMPUTE AUX-REAJUSTE-LTB =
                   AUX-REAJUSTE-LTB * AUX-PCALCULOPB-S


              MOVE AUX-REAJUSTE-LTB TO RSAL-PB-TECNICA

      *    IF AUX-IMPORTE-LTB-PBP < ZEROES
      *       MOVE ZEROES TO AUX-IMPORTE-LTB-PBP
      *    ELSE
      *       MOVE RSAL-DEV-NETCOASE
      *    END-IF
      *D*
      *    DISPLAY 'RSAL-PB-TECNICA  ' RSAL-PB-TECNICA
      *F*
           .
MF001 ******************************************************************10650099
MF001  REPARTIR-SINIESTRO.                                              10660099
MF001 ******************************************************************10670099
MF001 *D*
MF001 *    DISPLAY 'REPARTIR-SINIESTRO'
MF001 *F*
MF001 *    IF R350T077-FEMISION NOT = ZEROS
MF001 *       PERFORM VARYING I1 FROM 1 BY 1 UNTIL I1 > 6
MF001 *          IF R350T077-FEMISION <= TB-FHASTA (I1) AND
MF001 *             R350T077-FEMISION >  TB-FDESDE (I1)
MF001 *             ADD R350T077-IMPORTE-D
MF001 *                              TO TB-ACUMULADO-206 (I1)
MF001 *D*
MF001 *             DISPLAY '    REPARTIR-SINI1 ' R350T077-GRESPONSABLE
MF001 *                     ' TB-PERIODO (' I1 ') ' TB-PERIODO (I1)
MF001 *                     ' IMPORTE ' R350T077-IMPORTE-D
MF001 *F*
MF001 *          END-IF
MF001 *       END-PERFORM
MF001 *    ELSE
MF001 *       MOVE 'N' TO SW-PERIODO SW-PERIODO-B
MF001 *
MF001 *       PERFORM VARYING I1 FROM 1 BY 1 UNTIL I1 > 6
MF001 *            IF R350T077-FEFECTO <=  TB-FHASTA (I1) AND
MF001 *               R350T077-FEFECTO  >  TB-FDESDE (I1)
MF001 *               IF i1 < 5
MF001 *                  MOVE 'S'        TO SW-PERIODO
MF001 *               ELSE
MF001 *                  MOVE 'S'        TO SW-PERIODO-B
MF001 *               END-IF
MF001 *               ADD R350T077-IMPORTE-D
MF001 *                                  TO TB-ACUMULADO-206 (I1)
MF001 *D*
MF001 *              DISPLAY '    REPARTIR-SINI2 ' R350T077-GRESPONSABLE
MF001 *                      ' TB-PERIODO (' I1 ') ' TB-PERIODO (I1)
MF001 *                      ' IMPORTE ' R350T077-IMPORTE-D
MF001 *F*
MF001 *            END-IF
MF001 *       END-PERFORM
MF001 *       IF SW-NO-PERIODO
MF001 *D*
MF001 *              DISPLAY ' SW-NO-PERIODO     ' R350T077-GRESPONSABLE
MF001 *                      ' IMPORTE ' R350T077-IMPORTE-D
MF001 *                      ' efecto  ' R350T077-fefecto
MF001 *F*
MF001 *          IF SW-NO-LTP
mF001 *D*
MF001 *              DISPLAY '     sw-no-ltp     '
MF001 *f*
MF001 *             IF R350T077-FEFECTO >= TB-FDESDE (1) OR
MF001 *                TB-PERIODO (2) = SPACES
MF001 *                ADD R350T077-IMPORTE-D
MF001 *                                  TO TB-ACUMULADO-206 (1)
mF001 *D*
MF001 *              DISPLAY '     sw-no-ltp 1   '
MF001 *f*
MF001 *             ELSE
MF001 *                ADD R350T077-IMPORTE-D
MF001 *                                  TO TB-ACUMULADO-206 (2)
mF001 *D*
MF001 *              DISPLAY '     sw-no-ltp 3   '
MF001 *f*
MF001 *             END-IF
MF001 *          ELSE
MF001 *             IF R350T077-FEFECTO >= TB-FDESDE (3) OR
MF001 *                SW-NO-LTPA
MF001 *                ADD R350T077-IMPORTE-D
MF001 *                                  TO TB-ACUMULADO-206 (3)
mF001 *D*
MF001 *              DISPLAY '     sw-no-ltp 4   '
MF001 *f*
MF001 *             ELSE
MF001 *                ADD R350T077-IMPORTE-D
MF001 *                                  TO TB-ACUMULADO-206 (4)
mF001 *D*
MF001 *              DISPLAY '     sw-no-ltp 5   '
MF001 *f*
MF001 *             END-IF
MF001 *          END-IF
MF001 *       END-IF
MF001 *       IF (SW-SI-LTB OR SW-SI-LTBA) AND SW-NO-PERIODO-B
MF001 *D*
MF001 *              DISPLAY '     no-periodo-B  ' R350T077-GRESPONSABLE
MF001 *                      ' IMPORTE ' R350T077-IMPORTE-D
MF001 *                      ' efecto  ' R350T077-fefecto
MF001 *             IF R350T077-FEFECTO <= TB-FDESDE (6) AND
MF001 *                SW-SI-LTBA
MF001 *                ADD R350T077-IMPORTE-D
MF001 *                                  TO TB-ACUMULADO-206 (6)
mF001 *D*
MF001 *              DISPLAY '  6.GRABA BONOA ' R350T077-GRESPONSABLE
MF001 *f*
MF001 *             ELSE
MF001 *                IF SW-NO-LTBA AND
MF001 *                   R350T077-FEFECTO <= TB-FDESDE (5)
mF001 *D*
MF001 *              DISPLAY '  5.GRABA BONO  ' R350T077-GRESPONSABLE
MF001 *f*
MF001 *                ADD R350T077-IMPORTE-D
MF001 *                                  TO TB-ACUMULADO-206 (5)
MF001 *                END-IF
MF001 *             END-IF
MF001 *F*
MF001 *       END-IF
MF001 *    END-IF
MF001 *
MF001 *    .
MF001 *
MF001 *F*
MF001      IF R350T077-FEMISION NOT = ZEROS
MF001         PERFORM VARYING I1 FROM 1 BY 1 UNTIL I1 > 6
MF001            IF R350T077-FEMISION <= TB-FHASTA (I1) AND
MF001               R350T077-FEMISION >  TB-FDESDE (I1)
MF001               ADD R350T077-IMPORTE-D
MF001                                TO TB-ACUMULADO-206 (I1)
MF001 *D*
MF001 *             DISPLAY '    REPARTIR-SINI1 ' R350T077-GRESPONSABLE
MF001 *                     ' TB-PERIODO (' I1 ') ' TB-PERIODO (I1)
MF001 *                     ' IMPORTE ' R350T077-IMPORTE-D
MF001 *F*
MF001            END-IF
MF001         END-PERFORM
MF001      ELSE
MF001         MOVE 'N' TO SW-PERIODO SW-PERIODO-B
MF001
MF001         PERFORM VARYING I1 FROM 1 BY 1 UNTIL I1 > 6
MF001              IF R350T077-FEFECTO <=  TB-FHASTA (I1) AND
MF001                 R350T077-FEFECTO  >  TB-FDESDE (I1)
MF001                 IF I1 < 5
MF001                    MOVE 'S'        TO SW-PERIODO
MF001                 ELSE
MF001                    MOVE 'S'        TO SW-PERIODO-B
MF001                 END-IF
MF001                 ADD R350T077-IMPORTE-D
MF001                                    TO TB-ACUMULADO-206 (I1)
MF001 *D*
MF001 *              DISPLAY '    REPARTIR-SINI2 ' R350T077-GRESPONSABLE
MF001 *                      ' TB-PERIODO (' I1 ') ' TB-PERIODO (I1)
MF001 *                      ' IMPORTE ' R350T077-IMPORTE-D
MF001 *F*
MF001              END-IF
MF001         END-PERFORM
MF001         IF SW-NO-PERIODO
MF001 *D*
MF001 *              DISPLAY ' SW-NO-PERIODO     ' R350T077-GRESPONSABLE
MF001 *                      ' IMPORTE ' R350T077-IMPORTE-D
MF001 *                      ' efecto  ' R350T077-fefecto
MF001 *F*
MF001            IF SW-NO-LTP
mF001 *D*
MF001 *              DISPLAY '     sw-no-ltp     '
MF001 *f*
MF001               IF SW-NO-LTPA
mF001 *D*
MF001 *              DISPLAY '     sw-no-ltpa    '
MF001 *f*
MF001                  IF R350T077-FEFECTO >= TB-FDESDE (1) OR
MF001                     TB-PERIODO (2) = SPACES
MF001                     ADD R350T077-IMPORTE-D
MF001                                       TO TB-ACUMULADO-206 (1)
mF001 *D*
MF001 *                DISPLAY '  1.GRABA pb   ' R350T077-GRESPONSABLE
MF001 *f*
MF001                  ELSE
MF001                     ADD R350T077-IMPORTE-D
MF001                                       TO TB-ACUMULADO-206 (2)
mF001 *D*
MF001 *                 DISPLAY '  2.GRABA pban ' R350T077-GRESPONSABLE
MF001 *f*
MF001                  END-IF
MF001               ELSE
mF001 *D*
MF001 *              DISPLAY '     sw-si-ltpa    '
MF001 *f*
MF001                  ADD R350T077-IMPORTE-D
MF001                                       TO TB-ACUMULADO-206 (4)
mF001 *D*
MF001 *              DISPLAY '  3.GRABA plura  ' R350T077-GRESPONSABLE
MF001 *f*
MF001               END-IF
MF001            ELSE
MF001               IF R350T077-FEFECTO >= TB-FDESDE (3) OR
MF001                  SW-NO-LTPA
MF001                  ADD R350T077-IMPORTE-D
MF001                                    TO TB-ACUMULADO-206 (3)
mF001 *D*
MF001 *              DISPLAY '  5.GRABA pluri ' R350T077-GRESPONSABLE
MF001 *f*
MF001               ELSE
MF001                  ADD R350T077-IMPORTE-D
MF001                                    TO TB-ACUMULADO-206 (4)
mF001 *D*
MF001 *              DISPLAY '  6.GRABA plura ' R350T077-GRESPONSABLE
MF001 *f*
MF001               END-IF
MF001            END-IF
MF001         END-IF
MF001         IF (SW-SI-LTB OR SW-SI-LTBA) AND SW-NO-PERIODO-B
MF001 *D*
MF001 *              DISPLAY '     no-periodo-B  ' R350T077-GRESPONSABLE
MF001 *                      ' IMPORTE ' R350T077-IMPORTE-D
MF001 *                      ' efecto  ' R350T077-fefecto
MF001               IF R350T077-FEFECTO <= TB-FDESDE (6) AND
MF001                  SW-SI-LTBA
MF001                  ADD R350T077-IMPORTE-D
MF001                                    TO TB-ACUMULADO-206 (6)
mF001 *D*
MF001 *              DISPLAY '  7.GRABA BONOA ' R350T077-GRESPONSABLE
MF001 *f*
MF001               ELSE
MF001                  IF SW-NO-LTBA AND
MF001                     R350T077-FEFECTO <= TB-FDESDE (5)
mF001 *D*
MF001 *              DISPLAY '  8.GRABA BONO  ' R350T077-GRESPONSABLE
MF001 *f*
MF001                  ADD R350T077-IMPORTE-D
MF001                                    TO TB-ACUMULADO-206 (5)
MF001                  END-IF
MF001               END-IF
MF001 *F*
MF001         END-IF
MF001      END-IF
MF001 *
MF001      .
MF001
      ******************************************************************
       9001-R-FIN.
      ******************************************************************
      *
           FINISH.
      *
      ******************************************************************
       9000-FIN.
      ******************************************************************
      *D*
      *    DISPLAY '9000-FIN'
      *    DISPLAY '*************************'
      *    DISPLAY '***  B 3 5 0 J L 0 1  ***'
      *    DISPLAY '*************************'
      *F*
           CLOSE ENTRADA-CTEC
           CLOSE ENTRADA-MOVS
LTA        CLOSE ENTRADA-MOVS-LTA
PBTEC      CLOSE ENTRADA-FECHA
           CLOSE SALIDA-PBT
           CLOSE SALIDA-INC
           CLOSE SALIDA-TOT.
           GOBACK
           .
           COPY IDMS IDMS-STATUS.
        IDMS-ABORT.
            EXIT.
      ******************************************************************
