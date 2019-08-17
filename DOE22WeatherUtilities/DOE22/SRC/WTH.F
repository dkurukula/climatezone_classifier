      PROGRAM  WTHPRC                                                           WTHPRC   2
C              THIS PROGRAM PLAYS ALL THE GAMES WITH THE WEATHER FILES.         WTHPRC   3
C              THE INPUT FORMAT CONSISTS OF AN OVERALL RUN-TYPE CARD            WTHPRC   4
C              WHICH TELLS WHAT FACILITY OF THE WEATHR PROGRAM IS               WTHPRC   5
C              DESIRED.  THE CARDS FOLLOWING (IF ANY) ARE                       WTHPRC   6
C              DETERMINED BY THE TYPE OF RUN.                                   WTHPRC   7
C                                                                               WTHPRC   8
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /PARAMS/   STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 2
      LOGICAL            STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 3
C                                                                               /PARAMS/ 4
      COMMON  /PCKINT/   KYR,KSTAT,CN(12),TG(12),KTIM,KARD(500),KYRL            -089     1
C                                                                               /PCKINT/ 3
      COMMON  /REPORC/   IBEGM,IENDM                                            /REPORC/ 2
C                                                                               /REPORC/ 3
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /WEATHD/   IWDID(5),FIRSTW,LRECX,IWSOL,WLONG,WLAT,IWTZN,          /WEATHD/ 2
     1                   CLRNES,TGRND,IWYR,WBT,DBT,PATM,CLDAMT,ISNOW,           /WEATHD/ 3
     2                   IRAIN,IWNDDR,HUMRAT,DENSTY,ENTHAL,SOLRAD,              /WEATHD/ 4
     3                   DIRSOL,ICLDTY,WNDSPD,IRFCP,DPT                         /WEATHD/ 5
      LOGICAL FIRSTW                                                            /WEATHD/ 6
      DIMENSION          CALC(16)                                               /WEATHD/ 7
      EQUIVALENCE        (WBT,CALC(1))                                          /WEATHD/ 8
C                                                                               /WEATHD/ 9
      DIMENSION  ITYPS(4)                                                       WTHPRC  15
C                                                                               WTHPRC  16
      DATA  IBLANK   /4H    /                                                   WTHPRC  17
      DATA  NTYPS    /4/                                                        WTHPRC  18
      DATA  ITYPS    /4HPACK,4HSTAT,4HLIST,4HEDIT/                              WTHPRC  19
      DATA IEND/4HEND /                                                         WTHPRC  20
C                                                                               WTHPRC  21
 6000 FORMAT(1H1,47X,'DOE-2.2',A4,24H WEATHER UTILITY PROGRAM///                VER22    1
     1       57X,18HINPUT VERIFICATION//)                                       WTHPRC  23
 5000 FORMAT(A4)                                                                WTHPRC  24
 6001 FORMAT(1H ,9HRUN TYPE ,A4/1H ,13H-------------//)                         WTHPRC  25
 6002 FORMAT(1H ,14H--- ERROR --- ,A4,24H IS AN INVALID RUN TYPE.,              WTHPRC  26
     1       24H  PROCESSING TERMINATED.)                                       WTHPRC  27
C                                                                               WTHPRC  28
      CALL VERSION( VERS )                                                      -055     1
      NEWPAK = .FALSE.                                                          WTHPRC  29
      ITYPT  = 0                                                                WTHPRC  30
      NFILEW = 0                                                                WTHPRC  31
      INWTH  = 10                                                               WTHPRC  32
C>>>>>                                                                          WTHPRC  33
      OPEN( 5, FILE='INPUT.TMP', STATUS='old' )                                 WTHPRC  34
      OPEN( 6, FILE='OUTPUT', carriagecontrol = 'YES', STATUS='UNKNOWN')        WTHWF    1
      OPEN( 1, FILE='NEWTH.TMP', FORM='UNFORMATTED'                             WTHWF    2
     $                         , STATUS='UNKNOWN' ,                             WTHWF    3
     $      recordtype='variable',recl=6200,blocksize=148992)                   WTHWF    4
      OPEN(17,FILE='STOUT.TMP' , FORM='UNFORMATTED'                             WTHPRC  39
     .                         , STATUS='UNKNOWN' )                             WTHPRC  40
C>>>>>                                                                          WTHPRC  41
C              CHECK FOR ERRORS                                                 WTHPRC  42
   10 IF (STOPIT)  CALL ABT                                                     WTHPRC  43
C              INITIALIZE VARIABLES                                             WTHPRC  44
      KYRL = 0                                                                  -089     2
      ITYPTL = ITYPT                                                            WTHPRC  46
      INWTH  = 10                                                               WTHPRC  47
      DO 15 I=1,5                                                               WTHPRC  48
   15 IWDID(I) = IBLANK                                                         WTHPRC  49
      STOPIT = .FALSE.                                                          WTHPRC  50
      IBEGM  = 1                                                                WTHPRC  51
      FIRST  = .TRUE.                                                           WTHPRC  52
      FIRSTW = .TRUE.                                                           WTHPRC  53
      LRECX  = 0                                                                WTHPRC  54
C              GET RUN TYPE                                                     WTHPRC  55
      READ(INFIL,5000,END=4000) ITYP                                            WTHPRC  56
      IF (ITYP .EQ. IEND)  GO TO 4000                                           WTHPRC  57
C              PRINT HEADER                                                     WTHPRC  58
      WRITE(OUTFIL,6000) VERS                                                   WTHPRC  59
C              GET RUN TYPE NUMBER                                              WTHPRC  60
      ITYPT  = 0                                                                WTHPRC  61
      DO 20 I=1,NTYPS                                                           WTHPRC  62
      IF (ITYP .EQ. ITYPS(I))  ITYPT = I                                        WTHPRC  63
   20 CONTINUE                                                                  WTHPRC  64
C              PRINT OUT VERIFICATION LINE                                      WTHPRC  65
      WRITE(OUTFIL,6001) ITYP                                                   WTHPRC  66
C              CHECK FOR ERROR                                                  WTHPRC  67
      IF (ITYPT .EQ. 0)  GO TO 3000                                             WTHPRC  68
C              BRANCH ON RUN TYPE                                               WTHPRC  69
      GO TO (100,200,300,400), ITYPT                                            WTHPRC  70
  100 CONTINUE                                                                  WTHPRC  71
C              THE CODE COMMENTED OUT BELOW ALLOWS MULTIPLE PACKS               WTHPRC  72
C              AND EDITS IN A SINGLE DECK.  Q9SKPFF IS A BKY                    WTHPRC  73
C              SYSTEM ROUTINE THAT SKIPS TO END OF FILES.  THE OUTPUT           WTHPRC  74
C              WEATHER FILE CONTAINS PACKED WEATHER FILES SEPERATED             WTHPRC  75
C              BY EOFS.                                                         WTHPRC  76
      OPEN ( 10, FILE='WEATHR.TMP', FORM='FORMATTED'                            WTHWF    5
     .         ,RECL=1024,blocksize=132000,STATUS='UNKNOWN' )                   WTHWF    6
      NEWPAK = .TRUE.                                                           WTHPRC  79
C              NFILEW = NFILEW + 1                                              WTHPRC  80
      REWIND  OUTWTH                                                            WTHPRC  81
C              IF (NFILEW .EQ. 1)  GO TO 105                                    WTHPRC  82
C              CALL Q9SKPFF(OUTWTH,NFILEW-1)                                    WTHPRC  83
  105 CALL PACKER                                                               WTHPRC  84
      END FILE  OUTWTH                                                          WTHPRC  85
      GO TO 10                                                                  WTHPRC  86
  200 IF (.NOT. NEWPAK) GO TO 205                                               WTHPRC  87
      INWTH  = OUTWTH                                                           WTHPRC  88
      REWIND INWTH                                                              WTHPRC  89
C              IF (NFILEW .LE. 1)  GO TO 210                                    WTHPRC  90
C              CALL Q9SKPFF(INWTH,NFILEW-1)                                     WTHPRC  91
      GO TO 210                                                                 WTHPRC  92
  205 OPEN ( 10, FILE='WEATHR.TMP', FORM='UNFORMATTED'                          WTHWF    7
     $                            , STATUS='UNKNOWN' ,                          WTHWF    8
     $      recordtype='variable',recl=6200,blocksize=148992)                   WTHWF    9
  210 CALL STATS                                                                WTHPRC  95
      GO TO 10                                                                  WTHPRC  96
  300 IF (.NOT. NEWPAK)  GO TO 310                                              WTHPRC  97
      INWTH  = OUTWTH                                                           WTHPRC  98
      REWIND INWTH                                                              WTHPRC  99
C              IF (NFILEW .LE. 1)  GO TO 310                                    WTHPRC 100
C              CALL Q9SKPFF(INWTH,NFILEW-1)                                     WTHPRC 101
  310 CALL REPORT                                                               WTHPRC 102
      GO TO 10                                                                  WTHPRC 103
  400 CONTINUE                                                                  WTHPRC 104
      OPEN ( 10, FILE='WEATHR.TMP', FORM='UNFORMATTED'                          WTHWF   10
     $                            , STATUS='UNKNOWN' ,                          WTHWF   11
     $      recordtype='variable',recl=6200,blocksize=148992)                   WTHWF   12
      NEWPAK = .TRUE.                                                           WTHPRC 107
C              NFILEW = NFILEW + 1                                              WTHPRC 108
      REWIND  OUTWTH                                                            WTHPRC 109
C              IF (NFILEW .EQ. 1)  GO TO 405                                    WTHPRC 110
C              CALL  Q9SKPFF(OUTWTH,NFILEW-1)                                   WTHPRC 111
  405 CALL EDIT                                                                 WTHPRC 112
      END FILE  OUTWTH                                                          WTHPRC 113
      GO TO 10                                                                  WTHPRC 114
 3000 WRITE(OUTFIL,6002)ITYP                                                    WTHPRC 115
      CALL ABT                                                                  WTHPRC 116
 4000 CONTINUE                                                                  WTHPRC 117
      STOP                                                                      WTHPRC 118
      END                                                                       WTHPRC 119
      SUBROUTINE ABT                                                            ABT      2
C                                                                               ABT      3
C              ABT TERMINATES THE WEATHER PROCESSOR WHEN AN                     ABT      4
C              ERROR IS DETECTED.  AT BKY IT CALLS THE SYSTEM                   ABT      5
C              ROUTINE ABORT, WHICH PRODUCES A SHORT DUMP.  ON                  ABT      6
C              OTHER SYSTEMS STOP SHOULD BE USED.                               ABT      7
C                                                                               ABT      8
      PRINT 1                                                                   ABT      9
    1 FORMAT('??  **** USER ABORT ****')                                        ABT     10
      STOP                                                                      ABT     11
      END                                                                       ABT     12
      FUNCTION AMAX(B,N)                                                        AMAX     2
C                                                                               AMAX     3
C              THIS FUNCTION RETURNS THE MAXIMUM VALUE OF 'N' ELEMENTS          AMAX     4
C              OF ARRAY 'B'                                                     AMAX     5
C                                                                               AMAX     6
      DIMENSION B(N)                                                            AMAX     7
C                                                                               AMAX     8
      AMAX   = B(1)                                                             AMAX     9
      DO 1000 I = 2,N                                                           AMAX    10
      IF (B(I) .GT. AMAX)  AMAX = B(I)                                          AMAX    11
 1000 CONTINUE                                                                  AMAX    12
      RETURN                                                                    AMAX    13
      END                                                                       AMAX    14
      FUNCTION AMIN(B,N)                                                        AMIN     2
C                                                                               AMIN     3
C              THIS FUNCTION RETURNS THE MINIMUM VALUE OF 'N' ELEMENTS          AMIN     4
C              OF ARRAY 'B'                                                     AMIN     5
C                                                                               AMIN     6
      DIMENSION B(N)                                                            AMIN     7
C                                                                               AMIN     8
      AMIN   = B(1)                                                             AMIN     9
      DO 1000 I = 2,N                                                           AMIN    10
      IF (B(I) .LT. AMIN)  AMIN = B(I)                                          AMIN    11
 1000 CONTINUE                                                                  AMIN    12
      RETURN                                                                    AMIN    13
      END                                                                       AMIN    14
      FUNCTION AMOFCZ(CZ)                                                       AMOFCZ   2
      X      = 1. - CZ                                                          AMOFCZ   3
      X      = X * X                                                            AMOFCZ   4
      AMOFCZ = 1./(CZ + .02941176471*X*X)                                       AMOFCZ   5
      RETURN                                                                    AMOFCZ   6
      END                                                                       AMOFCZ   7
      FUNCTION AVG(IB,N)                                                        AVG      2
C                                                                               AVG      3
C              THIS FUNCTION RETURNS THE AVERAGE OF 'N' ITEMS OF                AVG      4
C              ARRAY 'IB'                                                       AVG      5
C                                                                               AVG      6
      DIMENSION  IB(N)                                                          AVG      7
C                                                                               AVG      8
      ISUM   = 0                                                                AVG      9
      DO 1000 I = 1,N                                                           AVG     10
      ISUM   = ISUM + IB(I)                                                     AVG     11
 1000 CONTINUE                                                                  AVG     12
      AVG    = FLOAT(ISUM)/FLOAT(N)                                             AVG     13
      RETURN                                                                    AVG     14
      END                                                                       AVG     15
      BLOCK DATA                                                                BLOCKD   2
C                                                                               BLOCKD   3
      COMMON  /CONST/    DTOR, PIOVR2, PIOVR4, PIOV12, IBLNK                    /CONST/  2
C                                                                               /CONST/  3
      COMMON  /EDTINT/   MON(12),CVALS(8),ICVALS(2)                             /EDTINT/ 2
      LOGICAL            MON                                                    /EDTINT/ 3
C                                                                               /EDTINT/ 4
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /LIMITS/   CZLIM(9)                                               /LIMITS/ 2
C                                                                               /LIMITS/ 3
      COMMON  /MONTHC/   BEFORE(12),MDAYS(12),MNAMES(12)                        /MONTHC/ 2
      INTEGER            BEFORE                                                 /MONTHC/ 3
C                                                                               /MONTHC/ 4
      COMMON  /PACKEC/   NTZ,ISTAT,XLAT,XLONG,                                  /PACKEC/ 2
     1                   IYR,INTINT,DDBT,IBEGH,NBS,IDOYSH(5),ISSHFT(5),         /PACKEC/ 3
     2                   ISHFT,ALT,SKYCOV(12),ILOC                              073a     1
C                                                                               /PACKEC/ 5
      COMMON  /PARAMS/   STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 2
      LOGICAL            STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 3
C                                                                               /PARAMS/ 4
      COMMON  /PCKINT/   KYR,KSTAT,CN(12),TG(12),KTIM,KARD(500),KYRL            -089     1
C                                                                               /PCKINT/ 3
      COMMON  /RAWDAT/   IDRY(24),IWET(24),IDEW(24),IPRESS(24),                 /RAWDAT/ 2
     1                   IWNDSP(24),ICLAMT(24),ISOL(24),IDN(24),                /RAWDAT/ 3
     2                   IWNDIR(24),ICLTY(24),IRN(24),ISN(24),                  CD144S   1
     3                   IOPQCA(24),IRELH(24),ICLTY1(24)                        CD144S   2
      DIMENSION IRAW(24,14)                                                     CD144S   3
      EQUIVALENCE        (IDRY(1),IRAW(1,1))                                    /RAWDAT/ 6
      COMMON /LSTIME/   LSTHRS(24)                                              /RAWDAT/ 7
C                                                                               /RAWDAT/ 8
      COMMON  /REPORC/   IBEGM,IENDM                                            /REPORC/ 2
C                                                                               /REPORC/ 3
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
      COMMON  /WEATHD/   IWDID(5),FIRSTW,LRECX,IWSOL,WLONG,WLAT,IWTZN,          /WEATHD/ 2
     1                   CLRNES,TGRND,IWYR,WBT,DBT,PATM,CLDAMT,ISNOW,           /WEATHD/ 3
     2                   IRAIN,IWNDDR,HUMRAT,DENSTY,ENTHAL,SOLRAD,              /WEATHD/ 4
     3                   DIRSOL,ICLDTY,WNDSPD,IRFCP,DPT                         /WEATHD/ 5
      LOGICAL FIRSTW                                                            /WEATHD/ 6
      DIMENSION          CALC(16)                                               /WEATHD/ 7
      EQUIVALENCE        (WBT,CALC(1))                                          /WEATHD/ 8
C                                                                               /WEATHD/ 9
C                                                                               BLOCKD  18
      DATA  BEFORE   /0,31,59,90,120,151,181,212,243,273,304,334/               BLOCKD  19
      DATA CN /1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1./                             BLOCKD  20
      DATA  CZLIM    /22., 20., 18., 15.6, 12.5, 10.4, 9., 8., 7.1853 /         BLOCKD  21
      DATA  DTOR     /.0174532925/                                              BLOCKD  22
      DATA  FIRSTW   /.TRUE./                                                   BLOCKD  23
      DATA  IBEGM    /1/                                                        BLOCKD  24
      DATA  IBLNK  /4H    /                                                     BLOCKD  25
      DATA  ICLAMT   /24*0/                                                     BLOCKD  26
      DATA  ICLTY   /24*0/                                                      BLOCKD  27
      DATA  IDEW     /24*0/                                                     BLOCKD  28
      DATA  IDRY     /24*0/                                                     BLOCKD  29
      DATA  IENDM    /12/                                                       BLOCKD  30
      DATA  IEOF    /.FALSE./                                                   BLOCKD  31
      DATA  IHOUR   /0/                                                         BLOCKD  32
      DATA  INFIL  /5/                                                          BLOCKD  33
      DATA  IPRESS   /24*0/                                                     BLOCKD  34
      DATA  ISOL     /24*0/                                                     BLOCKD  35
      DATA  IDN      /24*0/                                                     BLOCKD  36
      DATA  IRN      /24*0/                                                     BLOCKD  37
      DATA  ISN      /24*0/                                                     BLOCKD  38
      DATA  IUNDEF   /-999/                                                     BLOCKD  39
      DATA  IWET     /24*0/                                                     BLOCKD  40
      DATA  IWNDIR   /24*0/                                                     BLOCKD  41
      DATA  IWNDSP   /24*0/                                                     BLOCKD  42
      DATA KYR      /-999/                                                      BLOCKD  43
      DATA KSTAT    /-999/                                                      BLOCKD  44
      DATA  MDAYS    /31,28,31,30,31,30,31,31,30,31,30,31/                      BLOCKD  45
      DATA  MNAMES   /4HJAN ,4HFEB ,4HMAR ,4HAPR ,4HMAY ,4HJUN ,                BLOCKD  46
     1                4HJUL ,4HAUG ,4HSEP ,4HOCT ,4HNOV ,4HDEC  /               BLOCKD  47
      DATA  MON      /12*.FALSE./                                               BLOCKD  48
      DATA  OUTFIL  /6/                                                         BLOCKD  49
      DATA  OUTWTH  /1/                                                         BLOCKD  50
      DATA  PIOVR2   /1.570796327/                                              BLOCKD  51
      DATA  PIOVR4   / .7853981634/                                             BLOCKD  52
      DATA  PIOV12   / .2617993878/                                             BLOCKD  53
      DATA  SOLWTH  /11/                                                        BLOCKD  54
      DATA  STOPIT   /.FALSE./                                                  BLOCKD  55
      DATA  STOUT    /17/                                                       BLOCKD  56
      DATA  UNDEF    /-999.0/                                                   BLOCKD  57
      END                                                                       BLOCKD  59
      SUBROUTINE CANDCD                                                         CANDCD   2
C                                                                               CANDCD   3
C               DECODE CANADIAN WEATHER                                         CANDCD   4
C                                                                               CANDCD   5
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /PACKEC/   NTZ,ISTAT,XLAT,XLONG,                                  /PACKEC/ 2
     1                   IYR,INTINT,DDBT,IBEGH,NBS,IDOYSH(5),ISSHFT(5),         /PACKEC/ 3
     2                   ISHFT,ALT,SKYCOV(12),ILOC                              073a     1
C                                                                               /PACKEC/ 5
      COMMON  /PARAMS/   STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 2
      LOGICAL            STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 3
C                                                                               /PARAMS/ 4
      COMMON  /PCKINT/   KYR,KSTAT,CN(12),TG(12),KTIM,KARD(500),KYRL            -089     1
C                                                                               /PCKINT/ 3
      COMMON  /RAWDAT/   IDRY(24),IWET(24),IDEW(24),IPRESS(24),                 /RAWDAT/ 2
     1                   IWNDSP(24),ICLAMT(24),ISOL(24),IDN(24),                /RAWDAT/ 3
     2                   IWNDIR(24),ICLTY(24),IRN(24),ISN(24),                  CD144S   1
     3                   IOPQCA(24),IRELH(24),ICLTY1(24)                        CD144S   2
      DIMENSION IRAW(24,14)                                                     CD144S   3
      EQUIVALENCE        (IDRY(1),IRAW(1,1))                                    /RAWDAT/ 6
      COMMON /LSTIME/   LSTHRS(24)                                              /RAWDAT/ 7
C                                                                               /RAWDAT/ 8
      COMMON  /REPORC/   IBEGM,IENDM                                            /REPORC/ 2
C                                                                               /REPORC/ 3
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
      COMMON  /CONST/    DTOR, PIOVR2, PIOVR4, PIOV12, IBLNK                    /CONST/  2
C                                                                               /CONST/  3
      COMMON  /LOCALD/   STALAT,STALON,SSTALA,CSTALA,TSTALA,HRSLON              /LOCALD/ 2
C                                                                               /LOCALD/ 3
      COMMON  /MONTHC/   BEFORE(12),MDAYS(12),MNAMES(12)                        /MONTHC/ 2
      INTEGER            BEFORE                                                 /MONTHC/ 3
C                                                                               /MONTHC/ 4
      COMMON  /SKIPPY/   FIRSTS,NCALL                                           /SKIPPY/ 2
      LOGICAL            FIRSTS                                                 /SKIPPY/ 3
C                                                                               /SKIPPY/ 4
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
C                                                                               CANDCD  19
      DIMENSION DEABC(5), ITDRYC(24), ISHWM2(24), IWSKMH(24), IRHPCT(24)        CANDCD  20
      DIMENSION RKTS(24), DIRN(24), SOLHOR(24)                                  CANDCD  21
      REAL NEWSSM                                                               CANDCD  22
 9001 FORMAT(A4)                                                                CANDCD  23
 9002 FORMAT(I3,1X,F3.1,17X,24I3)                                               CANDCD  24
 9003 FORMAT(24I4)                                                              CANDCD  25
C               READ IN THE HEADER RECORD                                       CANDCD  26
      IF (FIRSTS) READ(INWTH,9001) IDUM                                         CANDCD  27
C               DAY OF YEAR                                                     CANDCD  28
      IDOY = BEFORE(IMNTH) + IDAY                                               CANDCD  29
      IMN = IMNTH                                                               CANDCD  30
      IF (IDAY .GT. MDAYS(IMNTH)) IMN = IMNTH+1                                 CANDCD  31
C               READ ONE DAY'S DATA                                             CANDCD  32
C               ITDRYC = DRY BULB TEMPERATURE IN DEGREES CENTIGRADE             CANDCD  33
C               ISHWM2 = TOTAL SOLAR RADIATION IN WATTS PER METER**2            CANDCD  34
C               IWSKMH = WIND SPEED IN KILOMETERS PER HOUR                      CANDCD  35
C               IRHPCT = RELATIVE HUMIDITY IN PERCENT                           CANDCD  36
C               GREF   = GROUND REFLECTANCE                                     CANDCD  37
C               IDOYIN = DAY OF YEAR FROM FILE                                  CANDCD  38
 3000 READ(INWTH,9002) IDOYIN,GREF,(ITDRYC(I),I=1,24)                           CANDCD  39
      READ(INWTH,9003) (ISHWM2(I),I=1,24)                                       CANDCD  40
      READ(INWTH,9003) (IWSKMH(I),I=1,24)                                       CANDCD  41
      READ(INWTH,9003) (IRHPCT(I),I=1,24)                                       CANDCD  42
C               CHECK FOR CORRECT DAY                                           CANDCD  43
      IF (IDOYIN .LT. IDOY)  GO TO 3000                                         CANDCD  44
C              SOLAR                                                            CANDCD  45
C                                                                               CANDCD  46
C              GET SOLAR CONSTANTS.  DEABC(1) IS THE SOLAR                      CANDCD  47
C              DECLINATION ANGLE; DEABC(2) IS THE EQUATION                      CANDCD  48
C              OF TIME.                                                         CANDCD  49
      CALL SUNPRM(IDOY,DEABC)                                                   CANDCD  50
C              GET SOLAR CONSTANT.  THIS FORMULA IS FROM DUFFIE AND             CANDCD  51
C              BECKMAN, PAGE 7.                                                 CANDCD  52
      SOLCON = 436.8*(1. + 0.033*COS(DTOR*360.*FLOAT(IDOY)/365.))               CANDCD  53
C              CALCULATE RKT                                                    CANDCD  54
      NEWSSM = 0.                                                               CANDCD  55
      SUNSUM = 0.                                                               CANDCD  56
      DO 1100 IH=1,24                                                           CANDCD  57
      ISOL(IH) = 0                                                              CANDCD  58
      IDN(IH) = 0                                                               CANDCD  59
      RKTS(IH) = 0.                                                             CANDCD  60
      SOLHOR(IH) = 0.                                                           CANDCD  61
      DIRN(IH) = 0.                                                             CANDCD  62
C              GET HOUR ANGLE.  ASSUME SOLAR DATA IS BINNED                     CANDCD  63
C              IN SOLAR TIME.                                                   CANDCD  64
      UL     = FLOAT(IH) - 12.                                                  CANDCD  65
      BL     = UL - 1.                                                          CANDCD  66
C              SUNRISE AND SUNSET HOUR ANGLES                                   CANDCD  67
      SSHA   = ACOS(-TAN(STALAT)*TAN(DEABC(1)))/PIOV12                          CANDCD  68
      SRHA   = -SSHA                                                            CANDCD  69
C              RESET BIN BOUNDARIES TO ALLOW FOR SUNRISE AND SET                CANDCD  70
      IF ((UL .LE. SRHA) .OR. (BL .GE. SSHA))  GO TO 1100                       CANDCD  71
      IF (SRHA .GT. BL)  BL = SRHA                                              CANDCD  72
      IF (SSHA .LT. UL)  UL = SSHA                                              CANDCD  73
      IF ((UL-BL) .LT. 0.02)  GO TO 1100                                        CANDCD  74
      IF (ISHWM2(IH) .EQ. 0)  GO TO 1100                                        CANDCD  75
      A      = SIN(DEABC(1))*SIN(STALAT)                                        CANDCD  76
      B      = COS(DEABC(1))*COS(STALAT)                                        CANDCD  77
C              INTEGRATE SOLAR Z DIREC. COSINE OVER BIN                         CANDCD  78
      COSZIN = A*(UL-BL) + B*(SIN(PIOV12*UL)-SIN(PIOV12*BL))/PIOV12             CANDCD  79
      SOLH   = SOLCON*COSZIN                                                    CANDCD  80
      SOLHM  = FLOAT(ISHWM2(IH))*0.31721                                        CANDCD  81
      SUNSUM = SUNSUM + SOLHM                                                   CANDCD  82
      RKTS(IH) = AMIN1(SOLHM/SOLH,0.9)                                          CANDCD  83
 1100 CONTINUE                                                                  CANDCD  84
C              REBIN SOLAR DATA                                                 CANDCD  85
      DO 1500 IH=1,24                                                           CANDCD  86
C              PUT LOCAL TIME IN TERMS OF HOUR ANGLE                            CANDCD  87
      UL     = FLOAT(IH) - 12. + FLOAT(NTZ) + DEABC(2) - XLONG/15.              CANDCD  88
      BL     = UL - 1.                                                          CANDCD  89
C              SET BIN EDGES OF OVERLAPPING SOLAR TIME BINS                     CANDCD  90
      SB2    = FLOAT(INT(UL))                                                   CANDCD  91
      IF (UL .LT. 0.0)  SB2 = SB2 - 1.                                          CANDCD  92
      SB1    = SB2 - 1.                                                         CANDCD  93
      SB3    = SB2 + 1.                                                         CANDCD  94
      IF ((UL .LE. SRHA) .OR. (BL .GE. SSHA))  GO TO 1500                       CANDCD  95
C              RESET BIN EDGES IF BINS OVERLAP SUNRISE OR SUNSET                CANDCD  96
      IF (SRHA .GT. BL)  BL = SRHA                                              CANDCD  97
      IF (SRHA .GT. SB1)  SB1 = SRHA                                            CANDCD  98
      IF (SSHA .LT. UL)  UL = SSHA                                              CANDCD  99
      IF (SSHA .LT. SB3)  SB3 = SSHA                                            CANDCD 100
C              INTERPOLATE RKT                                                  CANDCD 101
      ISB2   = INT(12.01+SB2)                                                   CANDCD 102
      ISB3   = ISB2 + 1                                                         CANDCD 103
      IF ((UL-BL) .LE. .02)  GO TO 1500                                         CANDCD 104
      IF ((SB2-SB1) .LT. .02)  RKT = RKTS(ISB3)                                 CANDCD 105
      IF ((SB3-SB2) .LT. .02)  RKT = RKTS(ISB2)                                 CANDCD 106
      IF (((SB2-SB1) .GE. .02) .AND. ((SB3-SB2) .GE. .02))                      CANDCD 107
     1   RKT = RKTS(ISB2)*(SB2-BL)/(UL-BL) + RKTS(ISB3)*(UL-SB2)                CANDCD 108
     2   /(UL-BL)                                                               CANDCD 109
      COSZIN = A*(UL-BL) + B*(SIN(PIOV12*UL)-SIN(PIOV12*BL))/PIOV12             CANDCD 110
      COSZAV = COSZIN/(UL-BL)                                                   CANDCD 111
      SOLH   = SOLCON*COSZIN                                                    CANDCD 112
      SOLHM  = SOLH*RKT                                                         CANDCD 113
      NEWSSM = NEWSSM + SOLHM                                                   CANDCD 114
C              GET DIFFUSE COMPONENT FROM ERBS, KLEIN, AND DUFFIE               CANDCD 115
C              CORRELATION                                                      CANDCD 116
      IF (RKT .LE. 0.22)  DIF = SOLHM*(1.-0.09*RKT)                             CANDCD 117
      RKT2   = RKT*RKT                                                          CANDCD 118
      IF ((RKT .GT. 0.22) .AND. (RKT .LE. 0.8))  DIF = SOLHM*                   CANDCD 119
     1   (.9511-.1604*RKT+4.388*RKT2-16.638*RKT*RKT2+12.336*RKT2*RKT2)          CANDCD 120
      IF (RKT .GT. 0.8)  DIF = 0.165*SOLHM                                      CANDCD 121
      DIRH   = AMAX1(0.,SOLHM-DIF)                                              CANDCD 122
      DIRN(IH) = DIRH/COSZAV                                                    CANDCD 123
      SOLHOR(IH) = SOLHM                                                        CANDCD 124
C              CHECK FOR MAXIMUM DIRECT NORMAL                                  CANDCD 125
      CALL MAXDIR(COSZAV,SOLCON,DIRMAX)                                         CANDCD 126
      DIRN(IH) = AMIN1(DIRN(IH),DIRMAX)                                         CANDCD 127
 1500 CONTINUE                                                                  CANDCD 128
C              RENORMALIZE AND STORE SOLAR DATA                                 CANDCD 129
      RATNRM = SUNSUM/NEWSSM                                                    CANDCD 130
      DO 1700 IH=1,24                                                           CANDCD 131
      DIRN(IH) = DIRN(IH)*RATNRM                                                CANDCD 132
      SOLHOR(IH) = SOLHOR(IH)*RATNRM                                            CANDCD 133
      IDN(IH) = IROUND(DIRN(IH))                                                CANDCD 134
      ISOL(IH) = IROUND(SOLHOR(IH))                                             CANDCD 135
 1700 CONTINUE                                                                  CANDCD 136
C               LOOP OVER THE HOURS IN THE DAY AND FILL THE /RAWDAT/ ARRAYS     CANDCD 137
      DO 2000 IH=1,24                                                           CANDCD 138
C               CONVERT DRYBULB FROM CENTIGRADE TO FAHRENHEIT                   CANDCD 139
          TDRYF = 1.8*FLOAT(ITDRYC(IH)) + 32.                                   CANDCD 140
C               SATURATED VAPOR PRESSURE                                        CANDCD 141
          PS = PPWVMS(TDRYF)                                                    CANDCD 142
C               PARTIAL PRESSURE                                                CANDCD 143
          PW = .01*FLOAT(IRHPCT(IH))*PS                                         CANDCD 144
C               ATMOSPHERIC PRESSURE; USE STANDARD PRESSURE CORRECTED FOR       CANDCD 145
C               ALTITUDE.  PRESSURE IN INCHES OF HG.                            CANDCD 146
          PRESHG = 29.92*EXP(-.0000368*ALT)                                     CANDCD 147
C               HUMIDITY RATIO                                                  CANDCD 148
          HUMRAT = .622*PW/(PRESHG-PW)                                          CANDCD 149
C               SPECIFIC ENTHALPY                                               CANDCD 150
          ENTH = .24*TDRYF + (1061.+.444*TDRYF)*HUMRAT                          CANDCD 151
C               WET BULB TEMPERATURE                                            CANDCD 152
          TWETF = WBF(ENTH,PRESHG)                                              CANDCD 153
C               DEW POINT TEMPERATURE                                           CANDCD 154
          Y = LOG(PW)                                                           CANDCD 155
          IF (PW .LE. .1836) THEN                                               CANDCD 156
              TDEWF = 71.98 + 24.873*Y + .8927*Y*Y                              CANDCD 157
          ELSE                                                                  CANDCD 158
              TDEWF = 79.047 + 30.579*Y + 1.8893*Y*Y                            CANDCD 159
          END IF                                                                CANDCD 160
C               WIND SPEED IN KNOTS                                             CANDCD 161
          WSKNOT = 0.54*FLOAT(IWSKMH(IH))                                       CANDCD 162
C               FILL THE DATA ARRAYS                                            CANDCD 163
          IDRY(IH) = IROUND(TDRYF)                                              CANDCD 164
          IWET(IH) = MIN0(IDRY(IH),IROUND(TWETF))                               CANDCD 165
          IDEW(IH) = MIN0(IWET(IH),IROUND(TDEWF))                               CANDCD 166
          IPRESS(IH) = IROUND(100.*PRESHG)                                      CANDCD 167
          IWNDSP(IH) = IROUND(WSKNOT)                                           CANDCD 168
          IWNDIR(IH) = 0                                                        CANDCD 169
          ICLAMT(IH) = IROUND(SKYCOV(IMN))                                      CANDCD 170
          ICLTY(IH) = 2                                                         CANDCD 171
          ICLTY1(IH) = 0                                                        CANDCD 172
          IRN(IH) = 0                                                           CANDCD 173
          ISN(IH) = 0                                                           CANDCD 174
 2000 CONTINUE                                                                  CANDCD 175
      RETURN                                                                    CANDCD 176
      END                                                                       CANDCD 177
      SUBROUTINE  CCFKS(IDOY,ITOC,ICA,ITCA,CCF,CC,P1)                           CCFKS    2
C              THIS ROUTINE CALCULATES THE KIMURU-STEPHENSON                    CCFKS    3
C              CLOUD COVER MODIFIER                                             CCFKS    4
      COMMON  /LOCALD/   STALAT,STALON,SSTALA,CSTALA,TSTALA,HRSLON              /LOCALD/ 2
C                                                                               /LOCALD/ 3
      DIMENSION  ITOC(4),ICA(4),P(4),Q(4),R(4)                                  CCFKS    5
C                                                                               CCFKS    6
      DATA  P/1.06,.96,.95,1.14/,Q/.012,.033,.030,.003/,                        CCFKS    7
     1R/-.0084,-.0106,-.0108,-.0082/                                            CCFKS    8
C                                                                               CCFKS    9
      ISEAS  = 4                                                                CCFKS   10
      IF (IDOY  .GE. 80 .AND. IDOY .LE. 171) ISEAS = 1                          CCFKS   11
      IF (IDOY .GE. 172 .AND. IDOY .LE. 263) ISEAS = 2                          CCFKS   12
      IF (IDOY .GE. 264 .AND. IDOY .LE. 354) ISEAS = 3                          CCFKS   13
      IF (STALAT.LT.0.)  ISEAS = MOD((ISEAS+1),4) + 1                           -092     2
      X      = 0.                                                               CCFKS   15
      DO  100  I = 1,4                                                          CCFKS   16
  100 IF (ITOC(I) .EQ. 8 .OR. ITOC(I) .EQ. 9)  X = X + FLOAT(ICA(I))            CCFKS   17
      CC     = FLOAT(ITCA) - .5*X                                               CCFKS   18
      CC     = AMIN1(10.,CC)                                                    CCFKS   19
      CC     = AMAX1( 0.,CC)                                                    CCFKS   20
      P1     = P(ISEAS)                                                         CCFKS   21
      CCF    = P(ISEAS) + Q(ISEAS)*CC + R(ISEAS)*CC*CC                          CCFKS   22
      RETURN                                                                    CCFKS   23
      END                                                                       CCFKS   24
      SUBROUTINE  CCM                                                           CCM      2
C                                                                               CCM      3
C              CALCULATES CLOUD COVER MODIFIER AS A FUNCTION OF CLOUD           CCM      4
C              TYPE , CLOUD AMOUNT, AND SOLAR ALTITUDE                          CCM      5
C                                                                               CCM      6
      COMMON /SUND/ISUNUP,GUNDOG,HORANG,TDECLN,EQTIME,SOLCON,                   /SUND/   2
     1        ATMEXT,SKYDFF,RAYCOS(3),RDN,BSUN,DECLN,CD,SD,                     /SUND/   3
     2        FSUNUP,CLDCOV,RDNCC,BSCC,DIFSOL,RTOT(4),SKYA                      /SUND/   4
      COMMON  /WEATHD/   IWDID(5),FIRSTW,LRECX,IWSOL,WLONG,WLAT,IWTZN,          /WEATHD/ 2
     1                   CLRNES,TGRND,IWYR,WBT,DBT,PATM,CLDAMT,ISNOW,           /WEATHD/ 3
     2                   IRAIN,IWNDDR,HUMRAT,DENSTY,ENTHAL,SOLRAD,              /WEATHD/ 4
     3                   DIRSOL,ICLDTY,WNDSPD,IRFCP,DPT                         /WEATHD/ 5
      LOGICAL FIRSTW                                                            /WEATHD/ 6
      DIMENSION          CALC(16)                                               /WEATHD/ 7
      EQUIVALENCE        (WBT,CALC(1))                                          /WEATHD/ 8
C                                                                               /WEATHD/ 9
C                                                                               CCM      9
      REAL  ICLD                                                                CCM     10
C                                                                               CCM     11
      EQUIVALENCE  (ICLD  , CLDAMT), (ICLTP  , ICLDTY)                          CCM     12
     1            ,(CLDCOV, CC    ), (AL    , RAYCOS(3))                        CCM     13
C                                                                               CCM     14
      SQ     = ICLD*ICLD                                                        CCM     15
      J      = ICLTP+1                                                          CCM     16
      IF( J-2 )  130, 100, 160                                                  CCM     17
C              CLOUD TYPE 1                                                     CCM     18
  100 IF( AL .GT. 0.707 )  GO TO 120                                            CCM     19
C              LOW SUN                                                          CCM     20
      CC     = .598+.00026*ICLD+.00021*SQ-.00035*ICLD*SQ                        CCM     21
      RETURN                                                                    CCM     22
C              HIGH SUN                                                         CCM     23
  120 CC     = .908-.03214*ICLD+.0102*SQ-.00114*ICLD*SQ                         CCM     24
      RETURN                                                                    CCM     25
C              CLOUD TYPE 0                                                     CCM     26
  130 IF( AL .GT. 0.707 )  GO TO 150                                            CCM     27
C              LOW SUN                                                          CCM     28
      CC     = .849-.01277*ICLD+.00360*SQ-.00059*ICLD*SQ                        CCM     29
      RETURN                                                                    CCM     30
C              HIGH SUN                                                         CCM     31
  150 CC     = 1.010-.01394*ICLD+.00553*SQ-.00068*ICLD*SQ                       CCM     32
      RETURN                                                                    CCM     33
C              CLOUD TYPE TWO                                                   CCM     34
  160 IF( AL .GT. 0.707 )  GO TO 180                                            CCM     35
C              LOW SUN                                                          CCM     36
      CC     = .724-.00625*ICLD+.00191*SQ-.00047*ICLD*SQ                        CCM     37
      RETURN                                                                    CCM     38
C              HIGH SUN                                                         CCM     39
  180 CC     = .959-.02304*ICLD+.00787*SQ-.00091*ICLD*SQ                        CCM     40
      RETURN                                                                    CCM     41
      END                                                                       CCM     42
      FUNCTION CCSLMT(OPQ,IRN,ILOC)                                             CCSLMT   2
C                                                                               CCSLMT   3
C              CALCULATE THE CLOUD COVER MODIFIER USING THE SOLMET              CCSLMT   4
C              REGRESSION FORMULAS (GERALD F. COTTON).                          CCSLMT   5
C                                                                               CCSLMT   6
C              OPQ    - OPAQUE CLOUD COVER (0 - 1)                              CCSLMT   7
C              IRN    - RAIN FLAG (1 IF RAINING; OTHERWISE 0)                   CCSLMT   8
C              ILOC   - ONE OF 26 LOCATIONS (1 - 26)                            CCSLMT   9
C                                                                               CCSLMT  10
C              ILOC     STATION NAME                                            CCSLMT  11
C              ----     ------------                                            CCSLMT  12
C                                                                               CCSLMT  13
C                1      ALBUQUERQUE, NM                                         CCSLMT  14
C                2      APALACHICOLA, FL                                        CCSLMT  15
C                3      BISMARCK, ND                                            CCSLMT  16
C                4      BOSTON, MA                                              CCSLMT  17
C                5      BROWNSVILLE, TX                                         CCSLMT  18
C                6      CAPE HATTERAS, NC                                       CCSLMT  19
C                7      CARIBOU, ME                                             CCSLMT  20
C                8      CHARLESTON, SC                                          CCSLMT  21
C                9      COLUMBIA, MO                                            CCSLMT  22
C               10      DODGE CITY, KS                                          CCSLMT  23
C               11      EL PASO, TX                                             CCSLMT  24
C               12      ELY, NV                                                 CCSLMT  25
C               13      FORT WORTH, TX                                          CCSLMT  26
C               14      FRESNO, CA                                              CCSLMT  27
C               15      GREAT FALLS, MT                                         CCSLMT  28
C               16      LAKE CHARLES, LA                                        CCSLMT  29
C               17      MADISON, WI                                             CCSLMT  30
C               18      MEDFORD, OR                                             CCSLMT  31
C               19      MIAMI, FL                                               CCSLMT  32
C               20      NASHVILLE, TN                                           CCSLMT  33
C               21      NEW YORK, NY                                            CCSLMT  34
C               22      NORTH OMAHA, NE                                         CCSLMT  35
C               23      PHOENIX, AZ                                             CCSLMT  36
C               24      SANTA MARIA, CA                                         CCSLMT  37
C               25      SEATTLE-TACOMA, WA                                      CCSLMT  38
C               26      WASHINGTON, DC                                          CCSLMT  39
C                                                                               CCSLMT  40
      REAL OPQ                                                                  CCSLMT  41
      INTEGER IRN, ILOC                                                         CCSLMT  42
C                                                                               CCSLMT  43
C              C0 - C4 ARE THE REGRESSION COEFFICIENTS FOR EACH LOCATION        CCSLMT  44
C                                                                               CCSLMT  45
      REAL C0(26), C1(26), C2(26), C3(26), C4(26)                               CCSLMT  46
C                                                                               CCSLMT  47
      DATA C0 / 1.0,1.0,1.0,1.01,1.01,1.0,1.01,1.0,1.0,1.0,1.0,1.0,1.0,         CCSLMT  48
     1          1.0,1.0,1.0,.999,1.0,1.01,1.01,1.0,1.0,1.0,1.0,1.0,1.0 /        CCSLMT  49
      DATA C1 / -.094,-.404,-.357,-.734,-.372,-.306,-.520,-.348,-.554,          CCSLMT  50
     1          -.349,-.214,-.192,-.390,-.369,-.325,-.409,-.425,-.332,          CCSLMT  51
     2          -.295,-.580,-.656,-.367,-.256,-.422,-.301,-.474 /               CCSLMT  52
      DATA C2 / -.296,.720,.781,1.260,.903,.667,1.350,.735,1.350,.801,          CCSLMT  53
     1          .346,.247,.730,.756,.228,1.050,.922,.634,.369,1.320,            CCSLMT  54
     2          1.300,.897,.361,1.390,.711,.920 /                               CCSLMT  55
      DATA C3 / -.182,-.838,-1.040,-1.230,-1.160,-.949,-1.460,-1.020,           CCSLMT  56
     1          -1.440,-1.080,-.666,-.602,-.970,-.974,-.522,-1.240,             CCSLMT  57
     2          -1.140,-.864,-.643,-1.390,-1.320,-1.150,-.667,-1.520,           CCSLMT  58
     3          -.998,-1.070 /                                                  CCSLMT  59
      DATA C4 / -.209,-.151,-.206,-.165,-.213,-.235,-.187,-.221,-.211,          CCSLMT  60
     1          -.219,-.230,-.210,-.214,-.177,-.178,-.246,-.186,-.153,          CCSLMT  61
     2          -.246,-.185,-.194,-.217,-.227,-.191,-.178,-.214 /               CCSLMT  62
C                                                                               CCSLMT  63
      CCSLMT = C0(ILOC) + C1(ILOC)*OPQ + C2(ILOC)*OPQ*OPQ +                     CCSLMT  64
     1         C3(ILOC)*OPQ*OPQ*OPQ + C4(ILOC)*FLOAT(IRN)                       CCSLMT  65
      RETURN                                                                    CCSLMT  66
      END                                                                       CCSLMT  67
      FUNCTION CLDGDY(IB,ITEMP,N)                                               CLDGDY   2
C                                                                               CLDGDY   3
C              THIS FUNCTION RETURNS THE NUMBER OF COOLING DEGREE DAYS          CLDGDY   4
C              OF 'N' ITEMS IN ARRAY 'IB' USING 'ITEMP' AS A BASE               CLDGDY   5
C                                                                               CLDGDY   6
C                                                                               CLDGDY   7
      DIMENSION  IB(24,1)                                                       CLDGDY   8
C                                                                               CLDGDY   9
      NDAYS=N/24                                                                CLDGDY  10
      CLDGDY=0.                                                                 CLDGDY  11
      DO 2000 IDAY=1,NDAYS                                                      CLDGDY  12
         IBMAX=IB(1,IDAY)                                                       CLDGDY  13
         IBMIN=IB(1,IDAY)                                                       CLDGDY  14
         DO 1000 I=2,24                                                         CLDGDY  15
            IBMAX=MAX0(IB(I,IDAY),IBMAX)                                        CLDGDY  16
            IBMIN=MIN0(IB(I,IDAY),IBMIN)                                        CLDGDY  17
 1000    CONTINUE                                                               CLDGDY  18
         AV=(FLOAT(IBMAX)+FLOAT(IBMIN))/2.                                      CLDGDY  19
         CLDGDY = CLDGDY + AMAX1(AV-FLOAT(ITEMP),0.)                            CLDGDY  20
 2000 CONTINUE                                                                  CLDGDY  21
      RETURN                                                                    CLDGDY  22
      END                                                                       CLDGDY  23
      FUNCTION CLDGHR(IB,ITEMP,N)                                               CLDGHR   2
C                                                                               CLDGHR   3
C              THIS FUNCTION RETURNS THE NUMBER OF COOLING DEG. HRS./24         CLDGHR   4
C              OF 'N' ITEMS IN ARRAY 'IB' USING 'ITEMP' AS A BASE               CLDGHR   5
C                                                                               CLDGHR   6
C                                                                               CLDGHR   7
      DIMENSION  IB(N)                                                          CLDGHR   8
C                                                                               CLDGHR   9
      CLDGHR = 0.0                                                              CLDGHR  10
      DO 1000 I = 1,N                                                           CLDGHR  11
      IDIF   = IB(I) - ITEMP                                                    CLDGHR  12
      IF(IDIF .GT. 0) CLDGHR = CLDGHR + FLOAT(IDIF)                             CLDGHR  13
 1000 CONTINUE                                                                  CLDGHR  14
      CLDGHR = CLDGHR/24.0                                                      CLDGHR  15
      RETURN                                                                    CLDGHR  16
      END                                                                       CLDGHR  17
      SUBROUTINE CTZDCD                                                         CTZDCD   2
C                                                                               CTZDCD   3
C              THIS ROUTINE DECODES ONE DAY OF CTZ, CD144, OR CD144             CTZDCD   4
C              WITH DATA EVERY THIRD HOUR (CD1443).                             CTZDCD   5
C                                                                               CTZDCD   6
      COMMON  /MONTHC/   BEFORE(12),MDAYS(12),MNAMES(12)                        /MONTHC/ 2
      INTEGER            BEFORE                                                 /MONTHC/ 3
C                                                                               /MONTHC/ 4
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /FILTYP/   IFTYP,IWSZ,ITPTYP                                      /FILTYP/ 2
C                                                                               /FILTYP/ 3
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /PACKEC/   NTZ,ISTAT,XLAT,XLONG,                                  /PACKEC/ 2
     1                   IYR,INTINT,DDBT,IBEGH,NBS,IDOYSH(5),ISSHFT(5),         /PACKEC/ 3
     2                   ISHFT,ALT,SKYCOV(12),ILOC                              073a     1
C                                                                               /PACKEC/ 5
      COMMON  /PARAMS/   STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 2
      LOGICAL            STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 3
C                                                                               /PARAMS/ 4
      COMMON  /PCKINT/   KYR,KSTAT,CN(12),TG(12),KTIM,KARD(500),KYRL            -089     1
C                                                                               /PCKINT/ 3
      COMMON  /RAWDAT/   IDRY(24),IWET(24),IDEW(24),IPRESS(24),                 /RAWDAT/ 2
     1                   IWNDSP(24),ICLAMT(24),ISOL(24),IDN(24),                /RAWDAT/ 3
     2                   IWNDIR(24),ICLTY(24),IRN(24),ISN(24),                  CD144S   1
     3                   IOPQCA(24),IRELH(24),ICLTY1(24)                        CD144S   2
      DIMENSION IRAW(24,14)                                                     CD144S   3
      EQUIVALENCE        (IDRY(1),IRAW(1,1))                                    /RAWDAT/ 6
      COMMON /LSTIME/   LSTHRS(24)                                              /RAWDAT/ 7
C                                                                               /RAWDAT/ 8
      COMMON  /REPORC/   IBEGM,IENDM                                            /REPORC/ 2
C                                                                               /REPORC/ 3
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
C                                                                               CTZDCD  17
      DIMENSION IRANGE(2,8),ITEMP(8),NWNDIR(16)                                 073b     3
C                                                                               CTZDCD  19
      DATA IRANGE / 47,49, 50,52, 36,38, 43,46, 41,42, 56,56,                   CTZDCD  20
     1              39,40, 58,58 /                                              CTZDCD  21
      DATA NWNDIR / 11, 12, 22, 32, 33, 34, 44, 54, 55, 56, 66,                 CTZDCD  22
     1              76, 77, 78, 88, 18 /                                        CTZDCD  23
      DATA IDASH  / 1H- /                                                       CTZDCD  24
      INTEGER ILEVCA(4),ICTCOL(4)                                               CD144S   5
      LOGICAL OPQDAT                                                            CD144S   6
      DATA ICTCOL / 58,63,69,75 /                                               CD144S   7
C                                                                               CTZDCD  25
      IF (IDAY.EQ.IDAYL) RETURN                                                 073b     4
C              IF INITIAL CALL                                                  073b     5
      IF (FIRST) THEN                                                           073b     6
C              READ FIRST CARD                                                  073b     7
  100     CALL GETCRD(INWTH,KARD,80)                                            073b     8
          IF (IEOF) RETURN                                                      073b     9
C              SEARCH FOR CORRECT STATION NUMBER                                073b    10
          IF (ITPTYP.EQ.10 .OR. ITPTYP.EQ.11) THEN                              073b    11
              KSTAT = NUMB2(KARD,1,5,80)                                        073b    12
          ELSE                                                                  073b    13
              KSTAT = NUMB2(KARD,6,7,80)                                        073b    14
          END IF                                                                073b    15
          IF (KSTAT.NE.ISTAT) GO TO 100                                         073b    16
C              CHECK FOR CORRECT YEAR                                           073b    17
          KYR = NUMB2(KARD,6,7,80)                                              073b    18
          IYR2 = IYR - (IYR/100)*100                                            073b    19
          IF (KYR.NE.IYR2 .AND. ITPTYP.NE.5) GO TO 100                          073b    20
C              CHECK FOR CORRECT MONTH                                          073b    21
          KMNTH = NUMB2(KARD,8,9,80)                                            073b    22
          IF (KMNTH.LT.IBEGM) GO TO 100                                         073b    23
          FIRST = .FALSE.                                                       073b    24
          KDAY = NUMB2(KARD,10,11,80)                                           073b    25
          KH = NUMB2(KARD,12,13,80)                                             073b    26
          KTIM = KMNTH*10000 + KDAY*100 + KH                                    073b    27
      END IF                                                                    073b    28
C              GET DATA FOR 1 DAY                                               073b    29
      DO 1000 IH=1,24                                                           073b    30
      ITIM = IMNTH*10000 + IDAY*100 + IH - 1                                    073b    31
C              IF NO DATA FOR THIS HOUR, SET THE DATA ARRAYS                    073b    32
C              TO *UNDEFINED*                                                   073b    33
      IF (ITIM.NE.KTIM) THEN                                                    073b    34
          DO I=1,12                                                             073b    35
              IRAW(IH,I) = IUNDEF                                               073b    36
          END DO                                                                073b    37
          ICLTY1(IH) = IUNDEF                                                   073b    38
          IOPQCA(IH) = IUNDEF                                                   CD144S   8
          IRELH(IH) = IUNDEF                                                    CD144S   9
          GO TO 1000                                                            073b    39
      END IF                                                                    073b    40
      IRN(IH) = 0                                                               CTZDCD  52
      ISN(IH) = 0                                                               CTZDCD  53
C              DECODE THE 3 TEMPERATURE FIELDS                                  -055     6
      DO I=1,3                                                                  -055     7
C              THE LEFTMOST COLUMN IN EACH FIELD IS OVERPUNCHED WITH            -055     8
C              THE SIGN.  CALL CTZOP TO DECODE IT.                              -055     9
          CALL CTZOP(KARD,IRANGE(1,I),LEFTDG,ISIGN,NDEF)                        -055    10
          IT = NUMB2(KARD,IRANGE(1,I)+1,IRANGE(2,I),80)                         -055    11
          IF (IT.EQ.IUNDEF) THEN                                                -055    12
              ITEMP(I) = IUNDEF                                                 -055    13
          ELSE                                                                  -055    14
              ITEMP(I) = ISIGN*(LEFTDG*100 + IT)                                -055    15
          END IF                                                                -055    16
      END DO                                                                    -055    17
C              DECODE THE NORMAL FIELDS WITH NUMB2                              -055    18
      ITEMP(4) = NUMB2(KARD,IRANGE(1,4),IRANGE(2,4),80)                         CD144S  10
      ITEMP(5) = NUMB2(KARD,IRANGE(1,5),IRANGE(2,5),80)                         CD144S  11
      ITEMP(7) = NUMB2(KARD,IRANGE(1,7),IRANGE(2,7),80)                         CD144S  12
C              CLOUD TYPE; A SINGLE OVERPUNCHED FIELD                           -055    22
      ITEMP(8) = ICTZCT(KARD,58)                                                CD144S  13
C              WINDSPEED; CHECK FOR > 99                                        -055    29
      IF (KARD(IRANGE(1,5)).EQ.IDASH .AND. KARD(IRANGE(2,5)).EQ.IDASH)          -055    30
     1    ITEMP(5) = 100                                                        -055    31
C              SET CLOUD TYPE                                                   CTZDCD  61
      ICLTY(IH) = ITEMP(8)                                                      CTZDCD  62
      ICLTY1(IH) = ITEMP(8)                                                     CTZDCD  63
      IF (ITEMP(8) .EQ. IUNDEF)  GO TO 300                                      CTZDCD  64
      ICT    = 0                                                                CTZDCD  65
      IT = IABS(ITEMP(8))                                                       -055    32
      IF (IT .LT. 8)  ICT = 2                                                   CTZDCD  67
      IF (IT .EQ. 2)  ICT = 1                                                   CTZDCD  68
      ICLTY(IH) = ICT                                                           CTZDCD  69
  300 CONTINUE                                                                  CTZDCD  70
C              WIND DIRECTION                                                   CTZDCD  71
      IF (ITEMP(5) .EQ. 0)  IWNDIR(IH) = 0                                      CTZDCD  72
      IF (ITEMP(5) .EQ. 0)  GO TO 600                                           CTZDCD  73
      IF (1900+KYR .GE. 1964)  GO TO 510                                        073b    41
      IF (IYR.GE.1964 .AND. ITPTYP.NE.5)  GO TO 510                             073b    42
      DO I=1,16                                                                 073b    43
          IF (NWNDIR(I) .EQ. ITEMP(7))  GO TO 500                               073b    44
      END DO                                                                    073b    45
      IWNDIR(IH) = IUNDEF                                                       CTZDCD  78
      GO TO 600                                                                 CTZDCD  79
  500 IWNDIR(IH) = I - 1                                                        CTZDCD  80
      GO TO 600                                                                 CTZDCD  81
  510 CONTINUE                                                                  CTZDCD  82
      IF (ITEMP(7) .EQ. IUNDEF)  IWNDIR(IH) = IUNDEF                            CTZDCD  83
      IF (ITEMP(7) .EQ. IUNDEF)  GO TO 600                                      CTZDCD  84
      IWNDIR(IH) = INT(FLOAT(ITEMP(7))*.44444444 + 0.5)                         CTZDCD  85
      IF (IWNDIR(IH) .EQ. 16)  IWNDIR(IH) = 0                                   CTZDCD  86
  600 CONTINUE                                                                  CTZDCD  87
C              CLOUD AMOUNT                                                     CTZDCD  88
      ITEMP(6) = ICTZCA(KARD,56)                                                CD144S  14
C              SET RAIN AND SNOW FLAGS                                          CTZDCD  91
      IR1 = NUMB2(KARD,25,25,80)                                                -055    33
      IR2 = NUMB2(KARD,26,26,80)                                                -055    34
      IF (IR1.LT.0 .AND. IR2.LT.0) IRN(IH)=IUNDEF                               -055    35
      IF (IR1.GT.0 .OR. IR2.GT.0)  IRN(IH)=1                                    -055    36
      IS1 = NUMB2(KARD,27,27,80)                                                -055    37
      IS2 = NUMB2(KARD,28,28,80)                                                -055    38
      IS3 = NUMB2(KARD,29,29,80)                                                -055    39
      IF (IS1.LT.0 .AND. IS2.LT.0 .AND. IS3.LT.0) ISN(IH)=IUNDEF                -055    40
      IF (IS1.GT.0 .OR. IS2.GT.0 .OR. IS3.GT.0)   ISN(IH)=1                     -055    41
C              MOVE DATA TO DATA ARRAYS                                         CTZDCD 108
      DO I=1,6                                                                  073b    46
          IRAW(IH,I) = ITEMP(I)                                                 073b    47
      END DO                                                                    073b    48
C              Relative Humidity (in Per Cent)                                  CD144S  15
      IRELH(IH) = NUMB2(KARD,53,55,80)                                          CD144S  16
      IOPQ = ICTZCA(KARD,79)                                                    CD144S  17
      ILEVCA(1) = ICTZCA(KARD,57)                                               CD144S  18
      ILEVCA(2) = ICTZCA(KARD,62)                                               CD144S  19
      ILEVCA(3) = ICTZCA(KARD,68)                                               CD144S  20
      ILEVCA(4) = ICTZCA(KARD,74)                                               CD144S  21
      ICA = ICTZCA(KARD,56)                                                     CD144S  22
      OPQDAT = .FALSE.                                                          CD144S  23
      COPQ = 0.                                                                 CD144S  24
      CTRANS = 0.                                                               CD144S  25
      IF (IOPQ.NE.IUNDEF) THEN                                                  CD144S  26
        OPQDAT = .TRUE.                                                         CD144S  27
        COPQ = FLOAT(IOPQ)                                                      CD144S  28
      ELSE IF (ILEVCA(1).NE.IUNDEF .AND. ILEVCA(2).NE.IUNDEF .AND.              CD144S  29
     1         ILEVCA(3).NE.IUNDEF .AND. ILEVCA(4).NE.IUNDEF) THEN              CD144S  30
        OPQDAT = .TRUE.                                                         CD144S  31
        DO I=1,4                                                                CD144S  32
          COPQ = COPQ + FLOAT(ILEVCA(I))                                        CD144S  33
          ILEVCT = ICTZCT(KARD,ICTCOL(I))                                       CD144S  34
          IF (ILEVCT.EQ.8 .OR. ILEVCT.EQ.9 .OR. ILEVCT.EQ.-9)                   CD144S  35
     1      CTRANS = CTRANS + FLOAT(ILEVCA(I))                                  CD144S  36
        END DO                                                                  CD144S  37
        COPQ = COPQ - 0.5*CTRANS                                                CD144S  38
      ELSE IF (ICA.NE.IUNDEF) THEN                                              CD144S  39
        OPQDAT = .TRUE.                                                         CD144S  40
        COPQ = FLOAT(ICA)                                                       CD144S  41
      END IF                                                                    CD144S  42
      COPQ = AMAX1(AMIN1(10.,COPQ),0.)                                          CD144S  43
      IF (OPQDAT) THEN                                                          CD144S  44
        IOPQCA(IH) = IROUND(COPQ)                                               CD144S  45
      ELSE                                                                      CD144S  46
        IOPQCA(IH) = IUNDEF                                                     CD144S  47
      END IF                                                                    CD144S  48
      ISOL(IH) = IUNDEF                                                         CD144S  49
      IDN(IH) = IUNDEF                                                          CD144S  50
C              GET THE NEXT DATA RECORD                                         073b    49
  900 CALL GETCRD(INWTH,KARD,80)                                                073b    50
C              IF END OF DATA, SET KTIM LARGE & RETURN                          073b    51
      IF (IEOF) THEN                                                            073b    52
          KTIM = 999999                                                         073b    53
          IEOF = .FALSE.                                                        073b    54
      ELSE                                                                      073b    55
          KMNTH = NUMB2(KARD,8,9,80)                                            073b    56
          KDAY = NUMB2(KARD,10,11,80)                                           073b    57
          KH = NUMB2(KARD,12,13,80)                                             073b    58
          KTIM = KMNTH*10000 + KDAY*100 + KH                                    073b    59
      END IF                                                                    073b    60
      IF (KMNTH.EQ.2 .AND. KDAY.EQ.29) GO TO 900                                073b    61
 1000 CONTINUE                                                                  CTZDCD 113
      IDAYL = IDAY                                                              073b    62
      RETURN                                                                    CTZDCD 115
      END                                                                       CTZDCD 116
      FUNCTION ICTZCA(KARD,ICOL)                                                ICTZCA   2
C                                                                               ICTZCA   3
C              Decode a cloud amount field in CD144 format                      ICTZCA   4
C                                                                               ICTZCA   5
      INTEGER KARD(80),ICOL                                                     ICTZCA   6
C                                                                               ICTZCA   7
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
      CALL CTZOP(KARD,ICOL,NUMB,ISIGN,NDEF)                                     ICTZCA   9
      IF (NDEF.EQ.IUNDEF) THEN                                                  ICTZCA  10
          ICTZCA = IUNDEF                                                       ICTZCA  11
      ELSE IF (NDEF.EQ.0 .AND. NUMB.EQ.0 .AND. ISIGN.EQ.-1) THEN                ICTZCA  12
C              A dash codes to cloud amount = 10                                ICTZCA  13
          ICTZCA = 10                                                           ICTZCA  14
      ELSE                                                                      ICTZCA  15
          ICTZCA = NUMB                                                         ICTZCA  16
      END IF                                                                    ICTZCA  17
      RETURN                                                                    ICTZCA  18
      END                                                                       ICTZCA  19
      FUNCTION ICTZCT(KARD,ICOL)                                                ICTZCT   2
C                                                                               ICTZCT   3
C              Decode a cloud type field in CD144 format                        ICTZCT   4
C                                                                               ICTZCT   5
      INTEGER KARD(80),ICOL                                                     ICTZCT   6
C                                                                               ICTZCT   7
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
      CALL CTZOP(KARD,ICOL,NUMB,ISIGN,NDEF)                                     ICTZCT   9
      IF (NDEF.EQ.IUNDEF) THEN                                                  ICTZCT  10
          ICTZCT = IUNDEF                                                       ICTZCT  11
      ELSE IF (NDEF.EQ.0 .AND. NUMB.EQ.0 .AND. ISIGN.EQ.-1) THEN                ICTZCT  12
C              A dash codes to "other obscuring phenomena";                     ICTZCT  13
C              we'll make it -1                                                 ICTZCT  14
          ICTZCT = -1                                                           ICTZCT  15
      ELSE                                                                      ICTZCT  16
          ICTZCT = ISIGN*NUMB                                                   ICTZCT  17
      END IF                                                                    ICTZCT  18
      RETURN                                                                    ICTZCT  19
      END                                                                       ICTZCT  20
      SUBROUTINE CTZOP(KARD,ICOL,NUMB,ISIGN,NDEF)                               CTZOP    2
C                                                                               CTZOP    3
C              DECODE AN OVERPUNCHED DIGIT ON A CTZ OR CD144 FILE               CTZOP    4
C                                                                               CTZOP    5
C              INPUTS                                                           CTZOP    6
C              ------                                                           CTZOP    7
C                                                                               CTZOP    8
C              KARD   - THE DATA RECORD                                         CTZOP    9
C              ICOL   - THE COLUMN OF OVERPUNCHED DIGIT                         CTZOP   10
C                                                                               CTZOP   11
C              OUTPUTS                                                          CTZOP   12
C              -------                                                          CTZOP   13
C                                                                               CTZOP   14
C              NUMB   - THE OUTPUT DIGIT (0-9, OR IUNDEF)                       CTZOP   15
C              ISIGN  - THE SIGN OF NUMB (-1 OR 1)                              CTZOP   16
C              NDEF   - NOT DEFINED FLAG (0 OR -999)                            CTZOP   17
C                                                                               CTZOP   18
      INTEGER KARD(80),ICOL,NUMB,ISIGN                                          CTZOP   19
C                                                                               CTZOP   20
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
C                                                                               CTZOP   23
      INTEGER INEG(10),INORM(10),IPOS(10),NUMBER(10)                            CTZOP   24
      INTEGER INDIG,IOK                                                         CTZOP   25
C                                                                               CTZOP   26
      DATA  IDASH    /1H-/                                                      CTZOP   27
      DATA  IBLNK    /1H /                                                      CTZOP   28
      DATA  IPLUS    /1H+/                                                      CTZOP   29
      DATA  INEG     /1H],1HJ,1HK,1HL,1HM,1HN,1HO,1HP,1HQ,1HR/                  CTZOP   30
      DATA  INORM    /1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9/                  CTZOP   31
      DATA  IPOS     /1H^,1HA,1HB,1HC,1HD,1HE,1HF,1HG,1HH,1HI/                  CTZOP   32
      DATA  NUMBER   /0,1,2,3,4,5,6,7,8,9/                                      CTZOP   33
C                                                                               CTZOP   34
      ISIGN = 1                                                                 CTZOP   35
      NUMB = 0                                                                  CTZOP   36
      NDEF = 0                                                                  CTZOP   37
      IOK = 0                                                                   CTZOP   38
      INDIG = KARD(ICOL)                                                        CTZOP   39
C              IF BLANK SET UNDEFINED FLAG AND LEAVE                            CTZOP   40
      IF (INDIG.EQ.IBLNK) THEN                                                  CTZOP   41
          NDEF = IUNDEF                                                         CTZOP   42
          RETURN                                                                CTZOP   43
      END IF                                                                    CTZOP   44
C              CHECK FOR + OR -, SET ISIGN AND RETURN                           CTZOP   45
      IF (INDIG.EQ.IPLUS) RETURN                                                CTZOP   46
      IF (INDIG.EQ.IDASH) THEN                                                  CTZOP   47
          ISIGN = -1                                                            CTZOP   48
          RETURN                                                                CTZOP   49
      END IF                                                                    CTZOP   50
C              DECODE THE DIGIT                                                 CTZOP   51
      IOK = 0                                                                   CTZOP   52
      DO I=1,10                                                                 CTZOP   53
          IF (INDIG.EQ.IPOS(I) .OR. INDIG.EQ.INORM(I)) THEN                     CTZOP   54
              NUMB = NUMBER(I)                                                  CTZOP   55
              IOK = 1                                                           CTZOP   56
              GO TO 100                                                         CTZOP   57
          END IF                                                                CTZOP   58
      END DO                                                                    CTZOP   59
      DO I=1,10                                                                 CTZOP   60
          IF (INDIG.EQ.INEG(I)) THEN                                            CTZOP   61
              NUMB = NUMBER(I)                                                  CTZOP   62
              ISIGN = -1                                                        CTZOP   63
              IOK = 1                                                           CTZOP   64
              GO TO 100                                                         CTZOP   65
          END IF                                                                CTZOP   66
      END DO                                                                    CTZOP   67
  100 CONTINUE                                                                  CTZOP   68
      IF (IOK.EQ.1) RETURN                                                      CTZOP   69
C              ILLEGAL CHARACTER; PRINT ERROR MESSAGE AND ABORT                 CTZOP   70
      WRITE (OUTFIL,9001) INDIG,(KARD(J),J=1,80)                                CTZOP   71
 9001 FORMAT(//' ERROR ENCOUNTERED - ROUTINE CTZOP'/                            CTZOP   72
     1       ' ILLEGAL CHARACTER IN CD144 WEATHER DATA'/                        CTZOP   73
     2       ' THE ILLEGAL CHARACTER IS: 'A1/                                   CTZOP   74
     3       ' THE DATA RECORD IS:'/                                            CTZOP   75
     4       1H ,80A1)                                                          CTZOP   76
      CALL ABT                                                                  CTZOP   77
      RETURN                                                                    CTZOP   78
      END                                                                       CTZOP   79
      FUNCTION CZAV(HORANG)                                                     CZAV     2
C                                                                               CZAV     3
C              CALCULATE THE AVERAGE OF THE ZENITH ANGLE OVER A ONE             CZAV     4
C              HOUR BIN, TAKING INTO ACCOUNT SUNRISE AND SUNSET.                CZAV     5
C                                                                               CZAV     6
      COMMON  /CONST/    DTOR, PIOVR2, PIOVR4, PIOV12, IBLNK                    /CONST/  2
C                                                                               /CONST/  3
      COMMON  /DAYPAR/   STSD,CTCD,SSS,CSC,HUNDOG,EQTIME,SOLCON,                /DAYPAR/ 2
     1                   TMSHFT,ISHFTH,WNXTHR,WPRVHR                            /DAYPAR/ 3
C                                                                               /DAYPAR/ 4
C                                                                               CZAV     9
      CZAV = 0.                                                                 CZAV    10
      IF ((HORANG-0.02) .LE. -HUNDOG)  RETURN                                   CZAV    11
      IF ((HORANG-.98) .GE. HUNDOG)  RETURN                                     CZAV    12
      BL = AMAX1(HORANG-1.,-HUNDOG)                                             CZAV    13
      UL = AMIN1(HORANG,HUNDOG)                                                 CZAV    14
      CSZINT = STSD*(UL-BL)+CTCD*(SIN(PIOV12*UL)-SIN(PIOV12*BL))/PIOV12         CZAV    15
      CZAV = CSZINT/(UL-BL)                                                     CZAV    16
      RETURN                                                                    CZAV    17
      END                                                                       CZAV    18
      SUBROUTINE DAYPR(IM,KDOM,KYR,ITIMZ)                                       DAYPAR   2
C              THIS SUBROUTINE COMPUTES PARAMETERS DEPENDENT ON THE             DAYPAR   3
C              EARTHS POSITION ABOUT THE SUN AND ASSUMED TO BE CONSTANT         DAYPAR   4
C              OVER A DAY.                                                      DAYPAR   5
C                                                                               DAYPAR   6
      COMMON  /CONST/    DTOR, PIOVR2, PIOVR4, PIOV12, IBLNK                    /CONST/  2
C                                                                               /CONST/  3
      COMMON  /DAYPAR/   STSD,CTCD,SSS,CSC,HUNDOG,EQTIME,SOLCON,                /DAYPAR/ 2
     1                   TMSHFT,ISHFTH,WNXTHR,WPRVHR                            /DAYPAR/ 3
C                                                                               /DAYPAR/ 4
      COMMON  /LOCALD/   STALAT,STALON,SSTALA,CSTALA,TSTALA,HRSLON              /LOCALD/ 2
C                                                                               /LOCALD/ 3
      COMMON  /MONTHC/   BEFORE(12),MDAYS(12),MNAMES(12)                        /MONTHC/ 2
      INTEGER            BEFORE                                                 /MONTHC/ 3
C                                                                               /MONTHC/ 4
      DATA E,SOLADJ,SAXTLT/.0167238,436.7,.3978500/                             -092     3
C     DATA RFRACT / -.01221700 /                                                -092     4
C              E = ECCENTRICITY OF EARTHS ELLIPTICAL ORBIT                      DAYPAR  12
C              SOLADJ = MEAN SOLCON / (1-E**2)                                  DAYPAR  13
C              MEAN SOLCON = 436.6 BTU/HR-FT2                                   DAYPAR  14
C              SAXTLT = SIN(23.44384 DEGREES) = SIN(TILT OF EARTHS AXIS)        DAYPAR  15
C              RFRACT = SIN(0.7 DEGS.) = SIN(REFRACTION OF HORIZ. BEAM)         DAYPAR  16
C                                                                               DAYPAR  17
C              CALCULATE POINT IN ANNUAL CYCLES.                                DAYPAR  18
C              AORBT1 = 0 ON DAY OF MIN SUN EARTH DISTANCE                      DAYPAR  19
C              AORBT2 = 0 ON 1965 JULIAN DAY 0                                  DAYPAR  20
C              AORBT3 = 0 ON DAY OF VERNAL EQUINOX                              DAYPAR  21
C                                                                               DAYPAR  22
C              THE GMT (GREENWICH MEAN TIME) NOON, IN THIS CENTURY, OF          DAYPAR  23
C              MINIMUM SUN-EARTH DISTANCE WAS 2 JANUARY 1965.  IF A LEAP        DAYPAR  24
C              YEAR HADNT BEEN SKIPPED (AROUND 1900) LAST CENTURYS              DAYPAR  25
C              MINIMUM SUN-EARTH DISTANCE WOULD HAVE BEEN 3 JANUARY 1841        DAYPAR  26
C              (NOTE 1965 - 1841 = 124).  THIS ALGORITHM IS VALID               DAYPAR  27
C              BETWEEN 1 MARCH 1900 AND 31 DECEMBER 1999 FOR TIME ZONES         DAYPAR  28
C              EAST OF THE INTERNATIONAL DATE LINE.                             DAYPAR  29
C                                                                               DAYPAR  30
C              MYRS   = 1900 + KYR - 1965 + 124                                 DAYPAR  31
      MYRS   = KYR + 59                                                         DAYPAR  32
      M4YRS  = MYRS/4                                                           DAYPAR  33
      MYREM  = MYRS - 4 * M4YRS                                                 DAYPAR  34
C              4 YEARS ARE 4 * 365 + 1 = 4 * 365.25 = 1461 DAYS.                DAYPAR  35
C              M4YRS*1461 CAN BE TOO BIG FOR 16 BITS, SO USE REAL               DAYPAR  36
C              NUMBER, BASDAS                                                   DAYPAR  37
      BASDAS = FLOAT(M4YRS)*1461. + MYREM * 365 + BEFORE(IM) + KDOM - 3         DAYPAR  38
      IF ((MYREM .GT. 2) .AND. (IM .GT. 2))  BASDAS = BASDAS + 1.               DAYPAR  39
      NORBTS = INT(BASDAS/365.242)                                              DAYPAR  40
      DOORBT = BASDAS - FLOAT(NORBTS) * 365.242                                 DAYPAR  41
C                                                                               DAYPAR  42
C              COMPUTE ELIPTICAL ORBIT ANGLE AND SOLAR INTENSITY AT             DAYPAR  43
C              CURRENT POSITION IN ORBIT.                                       DAYPAR  44
      AORBT1 = (4. * PIOVR2 * DOORBT + STALON) / 365.242                        DAYPAR  45
C              SOLCON = 436.6 BTU/HR-FT2 * ((1+E*COS(AORBT1))                   DAYPAR  46
C              /SQRT(1-E**2))**2                                                DAYPAR  47
      SOLCON = SOLADJ * (1. + E * COS(AORBT1))**2                               DAYPAR  48
C                                                                               DAYPAR  49
C              COMPUTE 1965-EQUIVALENT JULIAN DAY ANGLE AND EQUATION            DAYPAR  50
C              OF TIME.                                                         DAYPAR  51
      AORBT2 = (4. * PIOVR2 * (DOORBT + 1.) + STALON) / 365.242                 DAYPAR  52
      S1     = SIN(AORBT2)                                                      DAYPAR  53
      C1     = COS(AORBT2)                                                      DAYPAR  54
      S2     = SIN(2.*AORBT2)                                                   DAYPAR  55
      C2     = COS(2.*AORBT2)                                                   DAYPAR  56
      EQTIME = .123570 * S1 - .004289 * C1 + .153809 * S2 + .060783 * C2        DAYPAR  57
C                                                                               DAYPAR  58
C              COMPUTE MODIFIED SEASON ANGLE AND DECLINATION PARAMETERS.        DAYPAR  59
      AORBT3 = 4.885784 + .0334200 * S1 - .0013880 * C1 + .00034800 * S2        DAYPAR  60
     1             - .2827E-4 * C2 + AORBT2                                     DAYPAR  61
      SDECLN = SAXTLT * SIN(AORBT3)                                             DAYPAR  62
      CDECLN = COS(ASIN(SDECLN))                                                DAYPAR  63
      STSD   = SSTALA * SDECLN                                                  DAYPAR  64
      CTCD   = CSTALA * CDECLN                                                  DAYPAR  65
      TDECLN = SDECLN/CDECLN                                                    DAYPAR  66
      TDECL2 = TDECLN * TDECLN                                                  DAYPAR  67
      SD     = TDECLN * (1.- .16667*TDECL2)                                     DAYPAR  68
      CD     = 1.- .5*TDECL2                                                    DAYPAR  69
      SSS    = SSTALA * SD                                                      DAYPAR  70
      CSC    = CSTALA * CD                                                      DAYPAR  71
C              HUNDOG = ACOS((RFRACT - STSD) / CTCD) / PIOV12                   DAYPAR  72
      HUNDOG = ACOS( -STSD/CTCD ) / PIOV12                                      DAYPAR  73
C                                                                               DAYPAR  74
C              COMPUTE THE DIFFERENCE BETWEEN LOCAL SOLAR AND LOCAL             DAYPAR  75
C              STANDARD TIME.  NOTE THAT THE SIGN CONVENTION FOR EQTIME         DAYPAR  76
C              IS OPPOSITE TO THAT NORMALLY EMPLOYED (IN ASHRAE, DUFFIE         DAYPAR  77
C              AND BECKMAN, ETC.).                                              DAYPAR  78
C                                                                               DAYPAR  79
      TMSHFT = FLOAT(ITIMZ) - HRSLON - EQTIME                                   DAYPAR  80
      RETURN                                                                    DAYPAR  81
      END                                                                       DAYPAR  82
      SUBROUTINE DC9685                                                         DC9685   2
C                                                                               DC9685   3
C THIS SUBROUTINE DECODES ONE DAY OF WEATHER DATA IN                            DC9685   4
C TD9685 FORMAT AND STORES THE RESULTING DATA IN THE                            DC9685   5
C COMMON BLOCK /RAWDAT/                                                         DC9685   6
C                                                                               DC9685   7
      COMMON  /CONST/    DTOR, PIOVR2, PIOVR4, PIOV12, IBLNK                    /CONST/  2
C                                                                               /CONST/  3
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /FILTYP/   IFTYP,IWSZ,ITPTYP                                      /FILTYP/ 2
C                                                                               /FILTYP/ 3
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /LOCALD/   STALAT,STALON,SSTALA,CSTALA,TSTALA,HRSLON              /LOCALD/ 2
C                                                                               /LOCALD/ 3
      COMMON  /MONTHC/   BEFORE(12),MDAYS(12),MNAMES(12)                        /MONTHC/ 2
      INTEGER            BEFORE                                                 /MONTHC/ 3
C                                                                               /MONTHC/ 4
      COMMON  /PACKEC/   NTZ,ISTAT,XLAT,XLONG,                                  /PACKEC/ 2
     1                   IYR,INTINT,DDBT,IBEGH,NBS,IDOYSH(5),ISSHFT(5),         /PACKEC/ 3
     2                   ISHFT,ALT,SKYCOV(12),ILOC                              073a     1
C                                                                               /PACKEC/ 5
      COMMON  /PARAMS/   STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 2
      LOGICAL            STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 3
C                                                                               /PARAMS/ 4
      COMMON  /PCKINT/   KYR,KSTAT,CN(12),TG(12),KTIM,KARD(500),KYRL            -089     1
C                                                                               /PCKINT/ 3
      COMMON  /RAWDAT/   IDRY(24),IWET(24),IDEW(24),IPRESS(24),                 /RAWDAT/ 2
     1                   IWNDSP(24),ICLAMT(24),ISOL(24),IDN(24),                /RAWDAT/ 3
     2                   IWNDIR(24),ICLTY(24),IRN(24),ISN(24),                  CD144S   1
     3                   IOPQCA(24),IRELH(24),ICLTY1(24)                        CD144S   2
      DIMENSION IRAW(24,14)                                                     CD144S   3
      EQUIVALENCE        (IDRY(1),IRAW(1,1))                                    /RAWDAT/ 6
      COMMON /LSTIME/   LSTHRS(24)                                              /RAWDAT/ 7
C                                                                               /RAWDAT/ 8
      COMMON  /REPORC/   IBEGM,IENDM                                            /REPORC/ 2
C                                                                               /REPORC/ 3
      COMMON  /SKIPPY/   FIRSTS,NCALL                                           /SKIPPY/ 2
      LOGICAL            FIRSTS                                                 /SKIPPY/ 3
C                                                                               /SKIPPY/ 4
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
C                                                                               DC9685  22
C                                                                               DC9685  24
C FIND THE CORRECT STARTING POSITION IN THE INPUT WEATHER DATA                  DC9685  25
C FILE BY LOOKING FOR STATION NUMBER, YEAR, AND MONTH                           DC9685  26
C                                                                               DC9685  27
C INCREMENT THE CALL COUNT                                                      DC9685  28
      NCALL = NCALL + 1                                                         DC9685  29
      IF (NCALL .GT. 1)  GO TO 100                                              DC9685  30
C LOOK FOR CORRECT STATION                                                      DC9685  31
   10 CALL GETCRD(INWTH,KARD,65)                                                DC9685  32
      IF (IEOF) RETURN                                                          DC9685  33
      KSTAT = NUMB2(KARD,3,6,65)                                                -055    43
      IF (KSTAT .NE. ISTAT)  GO TO 10                                           DC9685  35
C LOOK FOR CORRECT YEAR                                                         DC9685  36
      IF (IYR .EQ. IUNDEF) GO TO 30                                             DC9685  37
   20 KYR = 1900 + NUMB2(KARD,7,8,65)                                           -055    44
      IF (KYR .EQ. IYR)  GO TO 30                                               DC9685  39
      CALL GETCRD(INWTH,KARD,65)                                                DC9685  40
      IF (IEOF) RETURN                                                          DC9685  41
      GO TO 20                                                                  DC9685  42
C FIND THE CORRECT MONTH                                                        DC9685  43
   30 KMON = NUMB2(KARD,9,10,65)                                                -055    45
      IF (KMON .GE. IBEGM)  GO TO 40                                            DC9685  45
      CALL GETCRD(INWTH,KARD,65)                                                DC9685  46
      IF (IEOF) RETURN                                                          DC9685  47
      GO TO 30                                                                  DC9685  48
   40 CONTINUE                                                                  DC9685  49
C                                                                               DC9685  50
C TD9685 DATA IS IN GREENWICH MEAN TIME.  WE NEED TO MOVE THROUGH               DC9685  51
C THE DATA TO FIND THE HOUR CORRESPONDING TO 1AM IN LOCAL TIME                  DC9685  52
C                                                                               DC9685  53
C GET THE GREENWICH MEAN TIME CORRESPONDING TO 1AM LOCAL TIME                   DC9685  54
C                                                                               DC9685  55
      IHGMT1 = 1 + NTZ                                                          DC9685  56
      IF (IHGMT1 .LE. 0)  IHGMT1 = IHGMT1 + 24                                  DC9685  57
C MOVE TO THIS HOUR, OR THE NEAREST HOUR JUST PAST IT (THE DATA                 DC9685  58
C IS TAKEN EVERY 3 HOURS, BUT THIS IS UNRELIABLE)                               DC9685  59
   50 KHGMT = NUMB2(KARD,13,14,65)                                              -055    46
      KDAY = NUMB2(KARD,11,12,65)                                               -055    47
      KHMNTH = (KDAY-1)*24 + KHGMT                                              DC9685  62
      IF (KHMNTH .GE. IHGMT1) GO TO 60                                          DC9685  63
      CALL GETCRD(INWTH,KARD,65)                                                DC9685  64
      IF (IEOF) RETURN                                                          DC9685  65
      GO TO 50                                                                  DC9685  66
   60 BACKSPACE INWTH                                                           DC9685  67
  100 CONTINUE                                                                  DC9685  68
C                                                                               DC9685  69
C REPEAT JAN 2 FOR TIME ZONES EAST OF GREENWICH                                 DC9685  70
C                                                                               DC9685  71
      IF ((IMNTH .EQ. 1) .AND. (IDAY .EQ. 2) .AND. (NTZ .LT. 0))                DC9685  72
     1   GO TO 1005                                                             DC9685  73
C                                                                               DC9685  74
C REPEAT DEC 30 FOR TIME ZONES WEST OF GREENWICH                                DC9685  75
C                                                                               DC9685  76
      IF ((IMNTH .EQ. 12) .AND. (IDAY .EQ. 30) .AND. (NTZ .GE. 0))              073b    63
     1   GO TO 1005                                                             DC9685  78
C                                                                               DC9685  79
C INITIALIZE THE DATA ARRAYS                                                    DC9685  80
C                                                                               DC9685  81
      DO 110 I=1,24                                                             DC9685  82
      DO 110 J=1,12                                                             DC9685  83
      IRAW(I,J) = IUNDEF                                                        DC9685  84
  110 CONTINUE                                                                  DC9685  85
C                                                                               DC9685  86
C GET DATA FOR ONE DAY.  FIRST GET FIRST RECORD.                                DC9685  87
C                                                                               DC9685  88
  190 CALL GETCRD(INWTH,KARD,65)                                                DC9685  89
      KH = NUMB2(KARD,13,14,65)                                                 -055    48
      KDAY = NUMB2(KARD,11,12,65)                                               -055    49
      KMON = NUMB2(KARD,9,10,65)                                                -055    50
C SKIP FEB 29                                                                   DC9685  93
      IF ((KMON .EQ. 2) .AND. (KDAY .EQ. 29))  GO TO 190                        DC9685  94
      KDOY = BEFORE(KMON) + KDAY                                                DC9685  95
      KHOY = KH + (KDOY-1)*24                                                   DC9685  96
C GET LOCAL STARTING HOUR OF THE YEAR                                           DC9685  97
      IHBEG = IHGMT1 + (KDOY-1)*24                                              DC9685  98
      IF (IHBEG .GT. KHOY)  IHBEG = IHBEG - 24                                  DC9685  99
      IHEND = IHBEG + 23                                                        DC9685 100
      GO TO 210                                                                 DC9685 101
C GET THE REST OF THE DATA RECORDS FOR THE DAY                                  DC9685 102
  200 CALL GETCRD(INWTH,KARD,65)                                                DC9685 103
      KH = NUMB2(KARD,13,14,65)                                                 -055    51
      KDAY = NUMB2(KARD,11,12,65)                                               -055    52
      KMON = NUMB2(KARD,9,10,65)                                                -055    53
C SKIP FEB 29                                                                   DC9685 107
      IF ((KMON .EQ. 2) .AND. (KDAY .EQ. 29))  GO TO 200                        DC9685 108
      KDOY = BEFORE(KMON) + KDAY                                                DC9685 109
      KHOY = KH + (KDOY-1)*24                                                   DC9685 110
      IF (KHOY .GT. IHEND)  GO TO 1000                                          DC9685 111
C GET THE DATA                                                                  DC9685 112
  210 KUNITS = NUMB2(KARD,15,15,65)                                             -055    54
      KDBT = NUMB2(KARD,29,30,65)                                               -055    55
      KDPT = NUMB2(KARD,36,37,65)                                               -055    56
      KCOV = NUMB2(KARD,16,16,65)                                               -055    57
      KCOVLM = NUMB2(KARD,31,31,65)                                             -055    58
      KWDIR = NUMB2(KARD,17,18,65)                                              -055    59
      KWSPD = NUMB2(KARD,19,20,65)                                              -055    60
      KPRESS = NUMB2(KARD,26,28,65)                                             -055    61
      KCLDTL = NUMB2(KARD,32,32,65)                                             -055    62
      KCLDTM = NUMB2(KARD,34,34,65)                                             -055    63
      KCLDTH = NUMB2(KARD,35,35,65)                                             -055    64
      KWEATH = NUMB2(KARD,23,24,65)                                             -055    65
      IH = KHOY - IHBEG + 1                                                     DC9685 125
C DEBUG PRINT                                                                   DC9685 126
C     IF (KDOY .LE. 5)                                                          DC9685 127
C    1 WRITE (OUTFIL,9000) KH,KDAY,KMON,KDOY,KHOY,IHBEG,IHEND,KUNITS,           DC9685 128
C    2 KDBT,KDPT,KCOV,KCOVLM,KWDIR,KWSPD,KPRESS,KCLDTL,KCLDTM,KCLDTH,           DC9685 129
C    3 IH,KWEATH                                                                DC9685 130
C9000 FORMAT(//1X,'KH     = ',I5,5X,'KDAY   = ',I5,5X,                          DC9685 131
C    1            'KMON   = ',I5,5X,'KDOY   = ',I5 / 1X,                        DC9685 132
C    2            'KHOY   = ',I5,5X,'IHBEG  = ',I5,5X,                          DC9685 133
C    3            'IHEND  = ',I5,5X,'KUNITS = ',I5 / 1X,                        DC9685 134
C    4            'KDBT   = ',I5,5X,'KDPT   = ',I5,5X,                          DC9685 135
C    5            'KCOV   = ',I5,5X,'KCOVLM = ',I5 / 1X,                        DC9685 136
C    6            'KWDIR  = ',I5,5X,'KWSPD  = ',I5,5X,                          DC9685 137
C    7            'KPRESS = ',I5,5X,'KCLDTL = ',I5 / 1X,                        DC9685 138
C    8            'KCLDTM = ',I5,5X,'KCLDTH = 'I5,5X,                           DC9685 139
C    9            'IH     = ',I5,5X,'KWEATH = 'I5 )                             DC9685 140
C PROCESS THE WINDSPEED                                                         DC9685 141
      IF (KWSPD .EQ. IUNDEF)  GO TO 300                                         DC9685 142
      IWNDSP(IH) = KWSPD                                                        DC9685 143
      IF ((KUNITS .EQ. 2) .OR. (KUNITS .EQ. 5))                                 DC9685 144
     +   IWNDSP(IH) = IROUND(.867*FLOAT(KWSPD))                                 DC9685 145
C PROCESS THE DRY BULB TEMPERATURE                                              DC9685 146
  300 IF (KDBT .EQ. IUNDEF)  GO TO 400                                          DC9685 147
      IDRY(IH) = KDBT                                                           DC9685 148
      DBT = FLOAT(KDBT)                                                         DC9685 149
      IF (KUNITS .EQ. 0)  GO TO 400                                             DC9685 150
      IF (KDBT .GT. 50)  KDBT = 50 - KDBT                                       DC9685 151
      DBT = 1.8*FLOAT(KDBT) + 32.                                               DC9685 152
      IDRY(IH) = IROUND(DBT)                                                    DC9685 153
C PROCESS THE DEWPOINT TEMPERATURE                                              DC9685 154
  400 IF (KDPT .EQ. IUNDEF)  GO TO 500                                          DC9685 155
      IDEW(IH) = KDPT                                                           DC9685 156
      DPT = FLOAT(KDPT)                                                         DC9685 157
      IF (KUNITS .EQ. 0)  GO TO 500                                             DC9685 158
      IF (KDPT .GT. 50)  KDPT = 50 - KDPT                                       DC9685 159
      DPT = 1.8*FLOAT(KDPT) + 32.                                               DC9685 160
      IDEW(IH) = IROUND(DPT)                                                    DC9685 161
C PROCESS WIND DIRECTION                                                        DC9685 162
  500 IF ((KWDIR .EQ. IUNDEF) .OR. (KWDIR .GT. 36)                              -092     5
     1     .OR. (KWSPD .LE. 0))  GO TO 600                                      DC9685 164
      IWNDIR(IH) = IROUND(FLOAT(KWDIR)*.444444)                                 DC9685 165
      IF (IWNDIR(IH) .EQ. 16)  IWNDIR(IH) = 0                                   DC9685 166
C PROCESS CLOUD COVER                                                           DC9685 167
  600 IF (KCOV .EQ. IUNDEF)  GO TO 700                                          DC9685 168
      IF ((KCOV .EQ. 0) .OR. (KCOV .EQ. 1))  ICLAMT(IH) = KCOV                  DC9685 169
      IF (KCOV .EQ. 2)  ICLAMT(IH) = 2 + MOD(KHOY,2)                            DC9685 170
      IF (( KCOV .EQ. 3) .OR. (KCOV .EQ. 4) .OR. (KCOV .EQ. 5))                 DC9685 171
     1   ICLAMT(IH) = KCOV + 1                                                  DC9685 172
      IF (KCOV .EQ. 6)  ICLAMT(IH) = 8 - MOD(KHOY,2)                            DC9685 173
      IF ((KCOV .EQ. 7) .OR. (KCOV .EQ. 8))  ICLAMT(IH) = KCOV + 2              DC9685 174
C PROCESS CLOUD TYPE                                                            DC9685 175
  700 ICLTY(IH) = 2                                                             DC9685 176
      IF ((KCLDTL .EQ. 6) .OR. (KCLDTL .EQ. 7)) ICLTY(IH) = 1                   DC9685 177
      IF ((KCOVLM .EQ. 0) .AND. (KCLDTH .GT. 0))  ICLTY(IH) = 0                 DC9685 178
      ICLTY1(IH) = KCLDTH*10000 + KCLDTM*100 + KCLDTL                           DC9685 179
C PROCESS PRESSURE                                                              DC9685 180
      IF (KPRESS .EQ. IUNDEF)  GO TO 800                                        DC9685 181
      IF (KPRESS .LT. 500) PRESMB = FLOAT(10000+KPRESS)*0.1                     DC9685 182
      IF (KPRESS .GE. 500) PRESMB = FLOAT(9000+KPRESS)*0.1                      DC9685 183
      PRESHG = PRESMB*0.02953                                                   DC9685 184
  800 IF (KPRESS .EQ. IUNDEF)  PRESHG = 29.92                                   WTH1     1
C ADJUST FOR ALTITUDE                                                           DC9685 185
      PRESS = PRESHG*EXP(-0.0000368*ALT)                                        DC9685 186
      IPRESS(IH) = IFIX(100.*PRESS + 0.5)                                       DC9685 187
C CALCULATE THE WETBULB TEMPERATURE                                             DC9685 188
      IF ((KDBT .EQ. IUNDEF) .OR. (KDPT .EQ. IUNDEF)) GO TO 900                 WTH1     2
      PPWV = PPWVMS(DPT)                                                        DC9685 191
      HUMRAT = .622*PPWV/(PRESS-PPWV)                                           DC9685 192
      ENTH = .24*DBT + (1061. + .444*DBT)*HUMRAT                                DC9685 193
      WBT = WBF(ENTH,PRESS)                                                     DC9685 194
      IWET(IH) = IROUND(WBT)                                                    DC9685 195
      IWET(IH) = MIN0(IWET(IH),IDRY(IH))                                        DC9685 196
      IDEW(IH) = MIN0(IDEW(IH),IWET(IH))                                        DC9685 197
C PROCESS THE WEATHER FLAGS.  SET THE RAIN AND SNOW FLAGS.                      DC9685 198
  900 IF (KWEATH .EQ. IUNDEF)  GO TO 950                                        DC9685 199
      IRN(IH) = 0                                                               DC9685 200
      ISN(IH) = 0                                                               DC9685 201
      IF ((KWEATH .GE. 50) .AND. (KWEATH .LE. 69))  IRN(IH)=1                   DC9685 202
      IF ((KWEATH .GE. 80) .AND. (KWEATH .LE. 84))  IRN(IH)=1                   DC9685 203
      IF ((KWEATH .GE. 87) .AND. (KWEATH .LE. 92))  IRN(IH)=1                   DC9685 204
      IF ((KWEATH .GE. 93) .AND. (KWEATH .LE. 99))  IRN(IH)=1                   DC9685 205
      IF ((KWEATH .GE. 68) .AND. (KWEATH .LE .79))  ISN(IH)=1                   DC9685 206
      IF ((KWEATH .GE. 83) .AND. (KWEATH .LE. 90))  ISN(IH)=1                   DC9685 207
      IF ((KWEATH .GE. 93) .AND. (KWEATH .LE. 94))  ISN(IH)=1                   DC9685 208
  950 CONTINUE                                                                  DC9685 209
      GO TO 200                                                                 DC9685 210
 1000 BACKSPACE INWTH                                                           DC9685 211
 1005 IDAYL = IDAY                                                              DC9685 212
C DEBUG PRINT                                                                   DC9685 213
C     IF (KDOY .LE. 6)                                                          DC9685 214
C    1 WRITE (OUTFIL,9001) ((IRAW(I,J),I=1,24),J=1,12)                          DC9685 215
C9001 FORMAT(//8I8/8I8/8I8)                                                     DC9685 216
      RETURN                                                                    DC9685 217
      END                                                                       DC9685 218
      SUBROUTINE EDIT                                                           EDIT     2
C                                                                               EDIT     3
C              THIS PROGRAM ALLOWS THE USER TO CHANGE A SPECIFIC VALUE          EDIT     4
C              ON A DOE PACKED WEATHR FILE.                                     EDIT     5
C                                                                               EDIT     6
C              INPUT                                                            EDIT     7
C              -----                                                            EDIT     8
C              TWO CARDS ARE REQUIRED FOR EACH INPUT. THEIR FORMATS ARE         EDIT     9
C              CARD   COLS    FORMAT    VARIABLE                                EDIT    10
C              ----   ----    ------    --------                                EDIT    11
C              1      1-6    I6        MONTH OF CHANGE                          EDIT    12
C              1      7-12   I6        DAY OF CHANGE                            EDIT    13
C              1     13-18   I6        HOUR OF CHANGE                           EDIT    14
C              2      1-6    F6.1      WET BULB TEMP                            EDIT    15
C              2      7-12   F6.1      DRY BULB TEMP                            EDIT    16
C              2     13-18   F6.1      DEW POINT TEMP                           EDIT    17
C              2     19-24   F6.1      ATMOSPHERIC PRESSURE                     EDIT    18
C              2     25-30   F6.1      WIND SPEED                               EDIT    19
C              2     31-36   F6.1      CLOUD AMOUNT                             EDIT    20
C              2     37-42   I6        CLOUD TYPE                               EDIT    21
C              2     43-48   I6        WIND DIRECTION                           EDIT    22
C              2     49-55   F6.1      TOTAL HORIZONTAL RADIATION               EDIT    23
C              2     56-62   F6.1      DIRECT NORMAL RADIATION                  EDIT    24
C              IF ANY FIELD HAS -999, IT IS ASSUMED THAT THE                    EDIT    25
C              VALUE FROM THE TAPE IS TO BE USED.  IT IS NOT NECESSARY          EDIT    26
C              THAT THE CHANGES BE INPUT IN NUMERICAL ORDER, AS THE             EDIT    27
C              PROGRAM SORTS THEM.  UP TO 336                                   EDIT    28
C              CHANGES CAN BE MADE IN ANY ONE RUN.                              EDIT    29
C                                                                               EDIT    30
C                   IF THE FIRST CARD HAS A MONTH NUMBER OF ZERO (0),           EDIT    31
C              THE PROGRAM ASSUMES THE NEXT TWO CARDS TO CONTAIN NEW            EDIT    32
C              CLEARNESS NUMBERS AND/OR GROUND TEMPERATURES.  THE               EDIT    33
C              FORMATS FOR THESE CARDS ARE                                      EDIT    34
C                                                                               EDIT    35
C              CARD      COLS      FORMAT    VARIABLE                           EDIT    36
C              ----      ----      ------    --------                           EDIT    37
C              1         1-6       I6        ZERO (0) OR BLANK                  EDIT    38
C              2         1-72      12F6.1    NEW CLEARNESS NUMBERS              EDIT    39
C              3         1-72      12F6.1    NEW GROUND TEMPERATURES (IN        EDIT    40
C                                            DEG F)                             EDIT    41
C                                                                               EDIT    42
C                   FOR CARDS 2 AND 3, THE FIELDS ARE FILLED IN FROM            EDIT    43
C                   LEFT TO RIGHT IN THE NUMERICAL ORDER OF THE MONTHS.         EDIT    44
C                   AS BEFORE, ANY FIELD WITH -999.0 IS UNCHANGED.              EDIT    45
C                   HOWEVER, BOTH CARDS MUST BE PRESENT, EVEN IF                EDIT    46
C                   ONLY ONE TYPE OF VALUE IS TO BE CHANGED.                    EDIT    47
C                                                                               EDIT    48
C              VARIABLES                                                        EDIT    49
C              ---------                                                        EDIT    50
C                                                                               EDIT    51
C              'CN'      = NEW CLEARNESS NUMBERS                                EDIT    52
C              'CVALS'   = CHANGED REAL VALUES FOR HOUR                         EDIT    53
C              'GROUND'  = FLAG FOR GROUND TEMPERATURE CHANGE                   EDIT    54
C              'ICVALS'  = CHANGED INTEGER VALUES FOR HOUR                      EDIT    55
C              'IM'      = INDEX FOR MONTH                                      EDIT    56
C              'IS'      = STARTING POINT FOR HOURLY INPUT.  1 IF NO            EDIT    57
C                          MONTHLY CHANGE, 2 IF MONTHLY CHANGE                  EDIT    58
C              'ISTART'  = STARTING POINT IN CHANGES FOR MONTHLY LOOP           EDIT    59
C              'ITIMES'  = TIME INPUT AND CHANGE TITLE ARRAY                    EDIT    60
C              'IVALS'   = INTEGER INPUT VALUES                                 EDIT    61
C              'MON'     = ARRAY OF FLAGS FOR EACH MONTH.  '.TRUE.' IF          EDIT    62
C                          CHANGES IN MONTH                                     EDIT    63
C              'NAMES'   = VARIABLE NAMES FOR ERROR MESSAGE PRINTOUT            EDIT    64
C              'NUMIN'   = NUMBER OF CHANGE READ IN                             EDIT    65
C              'NUMON'   = NUMBER OF CHANGES IN MONTH                           EDIT    66
C              'RVALS'   = REAL INPUT VALUES                                    EDIT    67
C              'TG'      = NEW GROUND TEMPERATURES                              EDIT    68
C              'UNDEF'   = VALUE TO BE FILLED IN UNCHANGED REAL FIELD           EDIT    69
C                                                                               EDIT    70
C              ARRAYS                                                           EDIT    71
C              ------                                                           EDIT    72
C                                                                               EDIT    73
C              'CNO' SIGNIFIES CHANGE NUMBER                                    EDIT    74
C                                                                               EDIT    75
C              CVALS(1) = RVLAS(1,CNO) = WET BULB TEMP                          EDIT    76
C                    2  =       2 =      DRY BULB TEMP                          EDIT    77
C                    3  =       3 =      DEW POINT TEMP                         EDIT    78
C                    4  =       4 =      ATMOSPHERIC PRESSURE                   EDIT    79
C                    5  =       5 =      WIND SPEED                             EDIT    80
C                    6  =       6 =      CLOUD AMOUNT                           EDIT    81
C                    7  =       7 =      TOT. HORIZ. SOLAR                      EDIT    82
C                    8  =       8 =      DIRECT NORMAL SOLAR                    EDIT    83
C                                                                               EDIT    84
C              ICVALS(1) = IVALS(1,CNO) = CLOUD TYPE                            EDIT    85
C                     2  =       2      = WIND DIRECTION                        EDIT    86
C                                                                               EDIT    87
C              ITIMES(1,CNO) = MONTH                                            EDIT    88
C                     2      = DAY OF MONTH                                     EDIT    89
C                     3      = HOUR OF DAY                                      EDIT    90
C                     4      = HOUR OF YEAR                                     EDIT    91
C                                                                               EDIT    92
C              EXTERNALS                                                        EDIT    93
C              ---------                                                        EDIT    94
C                                                                               EDIT    95
C              EOF(FILE) RETURNS A NON-ZERO VALUE IF AND END-OF-FILE HAS        EDIT    96
C                   BEEN ENCOUNTERED ON 'FILE' (SYSTEM)                         EDIT    97
C              EDTPCK CHECKS CHANGES FOR VALIDITY AND, IF VALID, MAKES          EDIT    98
C                CHANGES TO HOURLY VALUES AND PACKS THEM INTO THE               EDIT    99
C                APPROPRIATE WORD IN 'IDATA'                                    EDIT   100
C              SWAP   INTERCHANGES THE VALUES IN THE ELEMENTS OF AN             EDIT   101
C                ARRAY OF FORM 'XXX(N,336)' WITHOUT REGARD TO TYPE              EDIT   102
C                                                                               EDIT   103
C                                                                               EDIT   104
      COMMON  /EDTINT/   MON(12),CVALS(8),ICVALS(2)                             /EDTINT/ 2
      LOGICAL            MON                                                    /EDTINT/ 3
C                                                                               /EDTINT/ 4
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /FILTYP/   IFTYP,IWSZ,ITPTYP                                      /FILTYP/ 2
C                                                                               /FILTYP/ 3
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /IDAT/     IDAT(1536)                                             /IDAT/   2
C                                                                               /IDAT/   3
      COMMON  /MONTHC/   BEFORE(12),MDAYS(12),MNAMES(12)                        /MONTHC/ 2
      INTEGER            BEFORE                                                 /MONTHC/ 3
C                                                                               /MONTHC/ 4
      COMMON  /PARAMS/   STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 2
      LOGICAL            STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 3
C                                                                               /PARAMS/ 4
      COMMON  /SKRACH/   SKRACH(5000)                                           /SKRACH/ 2
C              USED BY EDIT, MONTHS, PACKER, STATS                              /SKRACH/ 3
C              ONLY EDIT USES ALL OF SKRACH                                     /SKRACH/ 4
C                                                                               /SKRACH/ 5
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
      COMMON  /WEATHD/   IWDID(5),FIRSTW,LRECX,IWSOL,WLONG,WLAT,IWTZN,          /WEATHD/ 2
     1                   CLRNES,TGRND,IWYR,WBT,DBT,PATM,CLDAMT,ISNOW,           /WEATHD/ 3
     2                   IRAIN,IWNDDR,HUMRAT,DENSTY,ENTHAL,SOLRAD,              /WEATHD/ 4
     3                   DIRSOL,ICLDTY,WNDSPD,IRFCP,DPT                         /WEATHD/ 5
      LOGICAL FIRSTW                                                            /WEATHD/ 6
      DIMENSION          CALC(16)                                               /WEATHD/ 7
      EQUIVALENCE        (WBT,CALC(1))                                          /WEATHD/ 8
C                                                                               /WEATHD/ 9
C                                                                               EDIT   116
      DIMENSION  CN(12)                                                         EDIT   117
      DIMENSION  IFILE(2), IBITS(2)                                             EDIT   118
      DIMENSION  ITIMES(4,336)                                                  EDIT   119
      DIMENSION  IVALS(2,336)                                                   EDIT   120
      DIMENSION  IWDSZ(2,2), IFLNAM(2,3)                                        EDIT   121
      DIMENSION  NAMES(3,2)                                                     EDIT   122
      DIMENSION  NAMS(4,2)                                                      EDIT   123
      DIMENSION  RVALS(8,336)                                                   EDIT   124
      DIMENSION  TG(12)                                                         EDIT   125
      DIMENSION  IDAT30(1536), IDAT60(768), IDATO(1488)                         EDIT   126
C                                                                               EDIT   127
      LOGICAL  GROUND                                                           EDIT   128
C                                                                               EDIT   129
      EQUIVALENCE  (ITIMES(1,1),SKRACH(1)),(IVALS(1,1),SKRACH(1345)),           EDIT   130
     1             (RVALS(1,1),SKRACH(2017))                                    EDIT   131
      EQUIVALENCE  (IDAT(1),IDAT30(1)),(IDAT(1),IDAT60(1)),                     EDIT   132
     1             (IDAT(1),IDATO(1))                                           EDIT   133
C                                                                               EDIT   134
      DATA  CN       /12*-999.0/                                                EDIT   135
      DATA  IEND     /4HEND /                                                   EDIT   136
      DATA  IFLNAM   /4HOLD ,2H  ,4HNORM,2HAL,4HSOLA,2HR /                      EDIT   137
      DATA  IS       /1/                                                        EDIT   138
      DATA  ISTART   /1/                                                        EDIT   139
      DATA  IWDSZ    /4H60-B,2HIT,4H30-B,2HIT/                                  EDIT   140
      DATA  MAXVAL   /336/                                                      EDIT   141
      DATA  NAMES    /4HCLEA,4HRNES,4HS   ,                                     EDIT   142
     1                4HGROU,4HND T,4HEMP /                                     EDIT   143
      DATA  NAMS     /4HWORD,4H SIZ,4HE   ,4H    ,                              EDIT   144
     1                4HFILE,4H TYP,4HE   ,4H    /                              EDIT   145
      DATA  NUMON    /0/                                                        EDIT   146
      DATA  TG       /12*-999.0/                                                EDIT   147
C                                                                               EDIT   148
 5000 FORMAT(3I6)                                                               EDIT   149
 5010 FORMAT(12F6.0)                                                            EDIT   150
 5020 FORMAT(6F6.1,2I6,2F6.2)                                                   EDIT   151
 5030 FORMAT(A4,A2,A4,A2)                                                       EDIT   152
 6000 FORMAT(1H ,16H--- CAUTION --- ,17HTOO MANY CHANGES.,                      EDIT   153
     1       30H  MAXIMUM 336 WILL BE CHANGED.)                                 EDIT   154
 6001 FORMAT(1H ,16H--- CAUTION --- ,24HNO CHANGE IN EDIT INPUT.)               EDIT   155
 6002 FORMAT(////1H ,14H--- ERROR --- ,24HNOT ENOUGH INPUT FOR RUN,             EDIT   156
     1           35H TYPE EDIT.  PROCESSING TERMINATED.)                        EDIT   157
 7010 FORMAT( 1H0 / 5H----- ,A3, 5H----- /                                      EDIT   158
     1       6X,46H  DAY   HR  VALUE                OLD       NEW /             EDIT   159
     2       6X,46H  ---   --  -----                ---       --- )             EDIT   160
 7020 FORMAT(8X, 2H-- ,3X, 3H--  ,3A4,2X,2F10.1)                                EDIT   161
 7030 FORMAT(                                                                   EDIT   162
     13X, 7H------- ,3X, 9H--  TGRND ,2X,2F10.1,5X,13H-----DATA OUT             EDIT   163
     2       ,31H OF RANGE  - OLD VALUE RETAINED )                              EDIT   164
 7040 FORMAT(26H1---THERE WERE CHANGES IN  ,I3,10H HOURS IN  ,I2,               EDIT   165
     17H MONTHS  )                                                              EDIT   166
C                                                                               EDIT   167
C              READ IN WORD SIZE AND FILE TYPE                                  EDIT   168
      READ(INFIL,5030,END=3100) IBITS,IFILE                                     EDIT   169
      IF (IBITS(1) .EQ. IEND)  GO TO 3100                                       EDIT   170
      CALL VALIDA(IBITS(1),IWDSZ(1,1),2,NAMS(1,1),STOPIT,IWSZ)                  EDIT   171
      CALL VALIDA(IFILE(1),IFLNAM(1,1),3,NAMS(1,2),STOPIT,IFTYP)                EDIT   172
C              READ NEXT CARD SEPERATELY TO CHECK FOR MONTHLY HEADER            EDIT   173
C              VALUE CHANGES                                                    EDIT   174
      READ (INFIL,5000) (ITIMES(J,1),J=1,3)                                     EDIT   175
      IF (ITIMES(1,1) .EQ. IUNDEF) GOTO 3080                                    EDIT   176
C                                                                               EDIT   177
C              CHECK IF FIRST CHANGE A MONTHLY HEADER VALUE CHANGE              EDIT   178
      IF (ITIMES(1,1) .NE. 0) GOTO 3000                                         EDIT   179
C                                                                               EDIT   180
C              READ NEW CLEARNESS NO. AND GROUND TEMP                           EDIT   181
      READ (INFIL,5010) CN,TG                                                   EDIT   182
      DO 1005 I=1,12                                                            EDIT   183
 1005 TG(I)  = TG(I) + 460.                                                     EDIT   184
      ITIMES(4,1) = 0                                                           EDIT   185
      GOTO 3010                                                                 EDIT   186
 3000 CONTINUE                                                                  EDIT   187
C                                                                               EDIT   188
C              READ HOURLY VALUES FROM FIRST CARD                               EDIT   189
      READ (INFIL,5020) (RVALS(J,1),J=1,6),(IVALS(J,1),J=1,2)                   EDIT   190
     1              ,(RVALS(J,1),J=7,8)                                         EDIT   191
      IS     = 2                                                                EDIT   192
 3010 CONTINUE                                                                  EDIT   193
C                                                                               EDIT   194
C              LOOP THROUGH INPUTS                                              EDIT   195
      DO 1010 I = IS,MAXVAL                                                     EDIT   196
      READ (INFIL,5000) (ITIMES(J,I),J=1,3)                                     EDIT   197
C                                                                               EDIT   198
C              CHECK FOR END OF INPUT                                           EDIT   199
      IF (ITIMES(1,I) .EQ. IUNDEF) GOTO 3020                                    EDIT   200
      READ (INFIL,5020) (RVALS(J,I),J=1,6),(IVALS(J,I),J=1,2)                   EDIT   201
     1              ,(RVALS(J,I),J=7,8)                                         EDIT   202
 1010 CONTINUE                                                                  EDIT   203
      I      = MAXVAL + 1                                                       EDIT   204
      WRITE(OUTFIL,6000)                                                        EDIT   205
C                                                                               EDIT   206
C              SKIP TO LAST INPUT CHANGE                                        EDIT   207
 3015 CONTINUE                                                                  EDIT   208
      READ (INFIL,5000) IJUNK                                                   EDIT   209
      IF (IJUNK .EQ. IUNDEF) GOTO 3018                                          EDIT   210
      READ (INFIL,5010) RJUNK                                                   EDIT   211
      GOTO 3015                                                                 EDIT   212
 3018 CONTINUE                                                                  EDIT   213
 3020 CONTINUE                                                                  EDIT   214
C                                                                               EDIT   215
C              SET NUMBER OF CHANGES                                            EDIT   216
      NUMIN  = I - 1                                                            EDIT   217
      IS     = 3 - IS                                                           EDIT   218
      ICHGX  = 1                                                                EDIT   219
C                                                                               EDIT   220
C              SET HOUR OF YEAR                                                 EDIT   221
      DO 1020 I = IS,NUMIN                                                      EDIT   222
      ITIMES(4,I) = (BEFORE(ITIMES(1,I))+ITIMES(2,I)-1)*24 + ITIMES(3,I)        EDIT   223
 1020 CONTINUE                                                                  EDIT   224
C                                                                               EDIT   225
C              SORT INPUTS BY HOUR OF YEAR                                      EDIT   226
      IF (NUMIN .LT. 2) GOTO 3025                                               EDIT   227
      NUMM1  = NUMIN - 1                                                        EDIT   228
      DO 1030 I = IS,NUMM1                                                      EDIT   229
      IP1    = I + 1                                                            EDIT   230
      DO 1030 J = IP1,NUMIN                                                     EDIT   231
      IF (ITIMES(4,I) .LE. ITIMES(4,J)) GOTO 1030                               EDIT   232
      CALL  SWAP(ITIMES,4,I,J)                                                  EDIT   233
      CALL  SWAP(IVALS,2,I,J)                                                   EDIT   234
      CALL  RSWAP(RVALS,8,I,J)                                                  WTH2     4
 1030 CONTINUE                                                                  EDIT   236
C                                                                               EDIT   237
C              LOOP THROUGH MONTHS, SETTING 'MON' FOR EACH                      EDIT   238
 3025 CONTINUE                                                                  EDIT   239
      DO 1040 I = 1,NUMIN                                                       EDIT   240
      MON(ITIMES(1,I)) = .TRUE.                                                 EDIT   241
 1040 CONTINUE                                                                  EDIT   242
      DO 1050 I = 1,12                                                          EDIT   243
      IF (CN(I) .NE. UNDEF .OR. TG(I) .NE. UNDEF) MON(I) = .TRUE.               EDIT   244
 1050 CONTINUE                                                                  EDIT   245
C                                                                               EDIT   246
C              LOOP THROUGH RECORDS                                             EDIT   247
      DO 1090 IREC=1,24                                                         EDIT   248
      IMNTH  = (IREC+1)/2                                                       EDIT   249
      IDAY   = MDAYS(IMNTH)                                                     EDIT   250
      IF (MOD(IREC,2) .EQ. 1)  IDAY = 16                                        EDIT   251
      IDAYX  = IDAY                                                             EDIT   252
      IHOUR  = 1                                                                EDIT   253
      IRECXO = IMNTH                                                            EDIT   254
      IF (IFTYP .GT. 1)  IRECXO = IREC                                          EDIT   255
C              FILL IDAT                                                        EDIT   256
      CALL  WDREAD                                                              EDIT   257
      IF (.NOT. MON(IMNTH)) GOTO 3070                                           EDIT   258
      IF (IDAY .NE. 16)  GO TO 3050                                             EDIT   259
      NUMON  = NUMON + 1                                                        EDIT   260
C                                                                               EDIT   261
C              PRINT MONTHLY HEADER                                             EDIT   262
      WRITE(OUTFIL,7010)MNAMES(IMNTH)                                           EDIT   263
C                                                                               EDIT   264
C              CHECK IF NEW CLEARNESS NO.                                       EDIT   265
      IF (CN(IMNTH) .EQ. UNDEF) GOTO 3030                                       EDIT   266
C                                                                               EDIT   267
C              PRINT CHANGE                                                     EDIT   268
      WRITE(OUTFIL,7020)(NAMES(I,1),I=1,3),CLRNES,CN(IMNTH)                     EDIT   269
 3030 CONTINUE                                                                  EDIT   270
C                                                                               EDIT   271
C              SET GROUND TEMP CHANGE FLAG                                      EDIT   272
      GROUND = .FALSE.                                                          EDIT   273
C                                                                               EDIT   274
C              CHECK IF NEW GROUND TEMP                                         EDIT   275
      IF (TG(IMNTH) .EQ. UNDEF) GOTO 3050                                       EDIT   276
C                                                                               EDIT   277
C                                                                               EDIT   278
C              PRINT CHANGE                                                     EDIT   279
      WRITE(OUTFIL,7020)(NAMES(I,2),I=1,3),TGRND,TG(IMNTH)                      EDIT   280
      GROUND = .TRUE.                                                           EDIT   281
 3050 CONTINUE                                                                  EDIT   282
C                                                                               EDIT   283
C              LOOP THROUGH INPUT                                               EDIT   284
      IF (ISTART .GT. NUMIN)  GO TO 3060                                        EDIT   285
      DO 1080 I = ISTART,NUMIN                                                  EDIT   286
C                                                                               EDIT   287
C              CHECK FOR NEW MONTH                                              EDIT   288
      IF (ITIMES(1,I) .GT. IMNTH) GOTO 3060                                     EDIT   289
      IF (ITIMES(2,I) .GT. IDAYX)  GO TO 3060                                   EDIT   290
      ICHGX  = ICHGX + 1                                                        EDIT   291
      IDAY   = ITIMES(2,I)                                                      EDIT   292
      IHOUR  = ITIMES(3,I)                                                      EDIT   293
      IDXO   = IDAY                                                             EDIT   294
      IF (IFTYP .GT. 1)  IDXO = MOD(IDAY-1,16) + 1                              EDIT   295
C                                                                               EDIT   296
C              SET CHANGED REAL VALUE ARRAY                                     EDIT   297
      DO 1060 J = 1,8                                                           EDIT   298
      CVALS(J) = RVALS(J,I)                                                     EDIT   299
 1060 CONTINUE                                                                  EDIT   300
C                                                                               EDIT   301
C              SET CHANGE INTEGER VALUE ARRAY                                   EDIT   302
      DO 1070 J = 1,2                                                           EDIT   303
      ICVALS(J) = IVALS(J,I)                                                    EDIT   304
 1070 CONTINUE                                                                  EDIT   305
C                                                                               EDIT   306
C              PUT NEW VALUES IN PACKED ARRAY                                   EDIT   307
      CALL  EDTPCK                                                              EDIT   308
 1080 CONTINUE                                                                  EDIT   309
 3060 CONTINUE                                                                  EDIT   310
C                                                                               EDIT   311
C              CHANGE NUMBER OF STARTING CHANGE FOR NEXT LOOP                   EDIT   312
      ISTART = ICHGX                                                            EDIT   313
C                                                                               EDIT   314
C              IF NEW HEADER VALUES, CHANGE                                     EDIT   315
      IF (CN(IMNTH) .NE. UNDEF) CLRNES = CN(IMNTH)                              EDIT   316
      IF (GROUND) TGRND = TG(IMNTH)                                             EDIT   317
 3070 CONTINUE                                                                  EDIT   318
C              WRITE PACKED BLOCK ON 'NEWTH'                                    EDIT   319
      IWSOL = 1 + (IFTYP-1)*2                                                   EDIT   320
      IWSP1=4                                                                   EDIT   321
      GO TO (3072,3072,3075,3078,3075,3078),IWSP1                               EDIT   322
 3072 IF (MOD(IREC,2) .EQ. 1)  GO TO 1090                                       EDIT   323
      WRITE (OUTWTH) IWDID(1),IWDID(2),IWYR,WLAT,WLONG,IWTZN,IMNTH,             EDIT   324
     1               MDAYS(IMNTH),CLRNES,TGRND,IWSOL,IDATO                      EDIT   325
      GO TO 1090                                                                EDIT   326
 3075 WRITE (OUTWTH) IWDID,IWYR,WLAT,WLONG,IWTZN,IRECXO,MDAYS(IMNTH),           EDIT   327
     1               CLRNES,TGRND,IWSOL,IDAT60                                  EDIT   328
      GO TO 1090                                                                EDIT   329
 3078 WRITE (OUTWTH) IWDID,IWYR,WLAT,WLONG,IWTZN,IRECXO,MDAYS(IMNTH),           EDIT   330
     1               CLRNES,TGRND,IWSOL,IDAT30                                  EDIT   331
 1090 CONTINUE                                                                  EDIT   332
C                                                                               EDIT   333
C              PRINT END SUMMARY                                                EDIT   334
      WRITE(OUTFIL,7040)NUMIN,NUMON                                             EDIT   335
      RETURN                                                                    EDIT   336
C                                                                               EDIT   337
C              WARN ABOUT NO CHANGES                                            EDIT   338
 3080 CONTINUE                                                                  EDIT   339
      WRITE(OUTFIL,6001)                                                        EDIT   340
      RETURN                                                                    EDIT   341
 3100 STOPIT = .TRUE.                                                           EDIT   342
      WRITE(OUTFIL,6002)                                                        EDIT   343
      RETURN                                                                    EDIT   344
      END                                                                       EDIT   345
      SUBROUTINE EDTPCK                                                         EDTPCK   2
C                                                                               EDTPCK   3
C              THIS SUBROUTINE USES 'WDREAD' TO UNPACK AN HOUR WHICH IS         EDTPCK   4
C              TO BE CHANGED, REPLACES THE VALUE(S), RECALCULATES NEW           EDTPCK   5
C              HUMRAT, ENTHAL, AND DENSITY IF NECESSARY, AND REPACKS IT.        EDTPCK   6
C                                                                               EDTPCK   7
C              VARIABLES                                                        EDTPCK   8
C              ---------                                                        EDTPCK   9
C                                                                               EDTPCK  10
C              'RCALC'   = TEMPORARY ARRAY FOR STORAGE OF OLD AND NEW           EDTPCK  11
C                          REAL VALUES                                          EDTPCK  12
C              'ICALC'   = TEMPORARY ARRAY FOR STORAGE OF OLD AND NEW           EDTPCK  13
C                          INTEGER VALUES                                       EDTPCK  14
C              'ILIMS'   = LIMITS ON INTEGER VALUES                             EDTPCK  15
C              'NAMES'   = NAMES OF HOURLY VARIABLES                            EDTPCK  16
C              'RLIMS'   = LIMITS ON REAL VALUES                                EDTPCK  17
C                                                                               EDTPCK  18
C              EXTERNALS                                                        EDTPCK  19
C              ---------                                                        EDTPCK  20
C                                                                               EDTPCK  21
C              PPWVMS  RETURNS VAPOR PRESSURE                                   EDTPCK  22
C                                                                               EDTPCK  23
C                                                                               EDTPCK  24
      COMMON  /EDTINT/   MON(12),CVALS(8),ICVALS(2)                             /EDTINT/ 2
      LOGICAL            MON                                                    /EDTINT/ 3
C                                                                               /EDTINT/ 4
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /FILTYP/   IFTYP,IWSZ,ITPTYP                                      /FILTYP/ 2
C                                                                               /FILTYP/ 3
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /IDAT/     IDAT(1536)                                             /IDAT/   2
C                                                                               /IDAT/   3
      COMMON  /PARAMS/   STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 2
      LOGICAL            STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 3
C                                                                               /PARAMS/ 4
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
      COMMON  /WEATHD/   IWDID(5),FIRSTW,LRECX,IWSOL,WLONG,WLAT,IWTZN,          /WEATHD/ 2
     1                   CLRNES,TGRND,IWYR,WBT,DBT,PATM,CLDAMT,ISNOW,           /WEATHD/ 3
     2                   IRAIN,IWNDDR,HUMRAT,DENSTY,ENTHAL,SOLRAD,              /WEATHD/ 4
     3                   DIRSOL,ICLDTY,WNDSPD,IRFCP,DPT                         /WEATHD/ 5
      LOGICAL FIRSTW                                                            /WEATHD/ 6
      DIMENSION          CALC(16)                                               /WEATHD/ 7
      EQUIVALENCE        (WBT,CALC(1))                                          /WEATHD/ 8
C                                                                               /WEATHD/ 9
C                                                                               EDTPCK  34
      DIMENSION  RCALC(8)                                                       EDTPCK  35
      DIMENSION  ICALC(2)                                                       EDTPCK  36
      DIMENSION  ILIMS(2,2)                                                     EDTPCK  37
      DIMENSION  IVALS(2)                                                       EDTPCK  38
      DIMENSION  NAMES(3,13),RLIMS(2,8),RVALS(8)                                EDTPCK  39
C                                                                               EDTPCK  40
      EQUIVALENCE  (IVALS(1),ICVALS(1))                                         EDTPCK  41
      EQUIVALENCE  (RVALS(1),CVALS(1))                                          EDTPCK  42
C                                                                               EDTPCK  43
      DATA  ILIMS    /0,15,0,15/                                                EDTPCK  44
      DATA  IZER     /0/                                                        EDTPCK  45
      DATA  NAMES    /4HWETB,4HULB ,4HTMP ,                                     EDTPCK  46
     1                4HDRYB,4HULB ,4HTMP ,                                     EDTPCK  47
     2                4HDEWP,4HOINT,4H TMP,                                     EDTPCK  48
     3                4HPRES,4HSURE,4H    ,                                     EDTPCK  49
     4                4HWIND,4H SPE,4HED  ,                                     EDTPCK  50
     5                4HCLOU,4HD AM,4HOUNT,                                     EDTPCK  51
     6                4HTOTA,4HL SO,4HLAR ,                                     EDTPCK  52
     7                4HDIR ,4HNORM,4H SOL,                                     EDTPCK  53
     8                4HCLOU,4HD TY,4HPE  ,                                     EDTPCK  54
     9                4HWIND,4H DIR,4H.   ,                                     EDTPCK  55
     1                4HHUMI,4HDITY,4H RAT,                                     EDTPCK  56
     2                4HENTH,4HALPY,4H    ,                                     EDTPCK  57
     3                4HDENS,4HITY ,4H    /                                     EDTPCK  58
      DATA  RLIMS    /-99.0,156.0,-99.0,156.0,-99.0,156.0,15.0,40.5,            EDTPCK  59
     1                0.0,127.0, 0.0,15.0, 0.0,511.0, 0.0,511.0/                EDTPCK  60
      DATA  RZER     /0.0/                                                      EDTPCK  61
C                                                                               EDTPCK  62
 6000 FORMAT(1H ,14H--- ERROR --- ,33HEND OF FILE READ ON WEATHER FILE.,        EDTPCK  63
     1       24H  PROCESSING TERMINATED.)                                       EDTPCK  64
 7000 FORMAT(6X,2I5,2X,3A4,2X,2F10.1,5X,2A10)                                   EDTPCK  65
 7010 FORMAT(4X, 5H----- ,I2,I5,2X,3A4,2X,2F10.1,15H ---DATA OUT OF,            EDTPCK  66
     1        8H RANGE ( ,F5.1, 1H, ,F5.1,21H)  OLD VALUE RETAINED )            EDTPCK  67
 7020 FORMAT(6X,2I5,2X,3A4,2I10)                                                EDTPCK  68
 7030 FORMAT(4X, 6H------ ,I2,I5,2X,3A4,2I10,15H ---DATA OUT OF                 EDTPCK  69
     1       ,8H RANGE ( ,I5, 1H, ,I5,21H)  OLD VALUE RETAINED )                EDTPCK  70
C                                                                               EDTPCK  71
C                                                                               EDTPCK  72
C              GET WEATHER DATA FOR HOUR                                        EDTPCK  73
      CALL  WDREAD                                                              EDTPCK  74
      IF (IEOF)  GO TO 9000                                                     EDTPCK  75
C              SET VALUES OF 'RCALC'                                            EDTPCK  76
      DPT    = UNDEF                                                            EDTPCK  77
      RCALC(1) = WBT                                                            EDTPCK  78
      RCALC(2) = DBT                                                            EDTPCK  79
      RCALC(3) = UNDEF                                                          EDTPCK  80
      RCALC(4) = PATM                                                           EDTPCK  81
      RCALC(5) = WNDSPD                                                         EDTPCK  82
      RCALC(6) = CLDAMT                                                         EDTPCK  83
      RCALC(7) = SOLRAD                                                         EDTPCK  84
      RCALC(8) = DIRSOL                                                         EDTPCK  85
C                                                                               EDTPCK  86
C              LOOP THROUGH REAL CHANGES                                        EDTPCK  87
      DO 1000 I = 1,8                                                           EDTPCK  88
C                                                                               EDTPCK  89
C              CHECK IF CHANGED                                                 EDTPCK  90
      IF (RVALS(I) .EQ. UNDEF) GOTO 1000                                        EDTPCK  91
C                                                                               EDTPCK  92
C              CHECK FOR VALIDITY                                               EDTPCK  93
      IF (RVALS(I) .LT. RLIMS(1,I) .OR. RVALS(I) .GT. RLIMS(2,I))               EDTPCK  94
     1             GOTO 3000                                                    EDTPCK  95
C                                                                               EDTPCK  96
C              CHANGE                                                           EDTPCK  97
      WRITE(OUTFIL,7000)IDAY,IHOUR,(NAMES(J,I),J=1,3),RCALC(I),RVALS(I)         EDTPCK  98
      RCALC(I) = RVALS(I) + RZER                                                EDTPCK  99
      GOTO 1000                                                                 EDTPCK 100
 3000 CONTINUE                                                                  EDTPCK 101
C                                                                               EDTPCK 102
C              PRINT ERROR                                                      EDTPCK 103
      WRITE(OUTFIL,7010)IDAY,IHOUR,(NAMES(J,I),J=1,3),RCALC(I),RVALS(I),        EDTPCK 104
     1           (RLIMS(J,I),J=1,2)                                             EDTPCK 105
 1000 CONTINUE                                                                  EDTPCK 106
C                                                                               EDTPCK 107
C              REPLACE WITH NEW VALUES                                          EDTPCK 108
      WBT    = RCALC(1)                                                         EDTPCK 109
      DBT    = RCALC(2)                                                         EDTPCK 110
      DPT    = RCALC(3)                                                         EDTPCK 111
      PATM   = RCALC(4)                                                         EDTPCK 112
      WNDSPD = RCALC(5)                                                         EDTPCK 113
      CLDAMT = RCALC(6)                                                         EDTPCK 114
      SOLRAD = RCALC(7)                                                         EDTPCK 115
      DIRSOL = RCALC(8)                                                         EDTPCK 116
C                                                                               EDTPCK 117
C              SET VALUES OF 'ICALC'                                            EDTPCK 118
      ICALC(1) = ICLDTY                                                         EDTPCK 119
      ICALC(2) = IWNDDR                                                         EDTPCK 120
C                                                                               EDTPCK 121
C              LOOP THROUGH INTEGER CHANGES                                     EDTPCK 122
      DO 1010 I = 1,2                                                           EDTPCK 123
C                                                                               EDTPCK 124
C              CHECK IF CHANGED                                                 EDTPCK 125
      IF (IVALS(I) .EQ. IUNDEF) GOTO 1010                                       EDTPCK 126
C                                                                               EDTPCK 127
C              CHECK FOR VALIDITY                                               EDTPCK 128
      IF (IVALS(I) .LT. ILIMS(1,I) .OR. IVALS(I) .GT. ILIMS(2,I))               EDTPCK 129
     1             GOTO 3010                                                    EDTPCK 130
C                                                                               EDTPCK 131
C              CHANGE                                                           EDTPCK 132
      WRITE(OUTFIL,7020)IDAY,IHOUR,(NAMES(J,I+8),J=1,3),                        EDTPCK 133
     1                  ICALC(I),IVALS(I)                                       EDTPCK 134
      ICALC(I) = IVALS(I) + IZER                                                EDTPCK 135
      GOTO 1010                                                                 EDTPCK 136
 3010 CONTINUE                                                                  EDTPCK 137
C                                                                               EDTPCK 138
C              PRINT ERROR                                                      EDTPCK 139
      WRITE(OUTFIL,7030)IDAY,IHOUR,(NAMES(J,I+8),J=1,3),                        EDTPCK 140
     1                  ICALC(I),IVALS(I),                                      EDTPCK 141
     2           (ILIMS(J,I),J=1,2)                                             EDTPCK 142
 1010 CONTINUE                                                                  EDTPCK 143
C                                                                               EDTPCK 144
C              REPLACE NEW VALUES                                               EDTPCK 145
      ICLDTY = ICALC(1)                                                         EDTPCK 146
      IWNDDR = ICALC(2)                                                         EDTPCK 147
C                                                                               EDTPCK 148
C              CHECK IF NEW DEW POINT GIVEN FOR RECALCULATION                   EDTPCK 149
      IF (DPT .EQ. UNDEF) GOTO 3040                                             EDTPCK 150
C                                                                               EDTPCK 151
C              CALUCULATE VAPOR PRESSURE                                        EDTPCK 152
      PPWV   = PPWVMS(DPT)                                                      EDTPCK 153
C                                                                               EDTPCK 154
C              RECALCULATE NEW HUMIDITY RATIO, DENSITY, AND ENTHALPY            EDTPCK 155
      HUM2   = 0.622*PPWV/(PATM-PPWV)                                           EDTPCK 156
      ENT2   = 0.24*DBT + (1061.0+0.444*DBT)*HUM2                               EDTPCK 157
      DEN2   = 1.0/(0.754*(DBT+459.7)*(1.0+1.606*HUM2)/PATM)                    EDTPCK 158
      WB2 = WBF(ENT2,PATM)                                                      EDTPCK 159
C                                                                               EDTPCK 160
C              PRINT CHANGES                                                    EDTPCK 161
      WRITE(OUTFIL,7000)IDAY,IHOUR,(NAMES(J,11),J=1,3),HUMRAT,HUM2              EDTPCK 162
      WRITE(OUTFIL,7000)IDAY,IHOUR,(NAMES(J,12),J=1,3),ENTHAL,ENT2              EDTPCK 163
      WRITE(OUTFIL,7000)IDAY,IHOUR,(NAMES(J,13),J=1,3),DENSTY,DEN2              EDTPCK 164
      WRITE(OUTFIL,7000)IDAY,IHOUR,(NAMES(J,1),J=1,3),RCALC(1),WB2              EDTPCK 165
C                                                                               EDTPCK 166
C              REPLACE NEW VALUES                                               EDTPCK 167
      HUMRAT = HUM2                                                             EDTPCK 168
      ENTHAL = ENT2                                                             EDTPCK 169
      DENSTY = DEN2                                                             EDTPCK 170
      WBT = WB2                                                                 EDTPCK 171
 3040 CONTINUE                                                                  EDTPCK 172
C              PACK WEATHD VARIABLES INTO IDAT                                  EDTPCK 173
      CALL WPACK                                                                EDTPCK 174
      RETURN                                                                    EDTPCK 175
 9000 STOPIT = .TRUE.                                                           EDTPCK 176
      WRITE(OUTFIL,6000)                                                        EDTPCK 177
      RETURN                                                                    EDTPCK 178
      END                                                                       EDTPCK 179
      SUBROUTINE EVALSN(NUMB,ISGN)                                              EVALSN   2
C                                                                               EVALSN   3
C              THIS ROUTINE DECODES A SIGNED FIELD FROM A WX1440 FILE.          EVALSN   4
C              IT WAS STOLEN FROM THE 'TDF-14 SURFACE OBSERVATIONS'             EVALSN   5
C              MANUAL                                                           EVALSN   6
C                                                                               EVALSN   7
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
C                                                                               EVALSN   9
      DIMENSION  INEG(10)                                                       EVALSN  10
      DIMENSION  INORM(10)                                                      EVALSN  11
      DIMENSION  IPOS(10)                                                       EVALSN  12
      DIMENSION  NUMBER(10)                                                     EVALSN  13
C                                                                               EVALSN  14
      DATA  IDASH    /1H-/                                                      EVALSN  15
      DATA  IBLNK    /1H /                                                      EVALSN  16
      DATA  INEG     /1H],1HJ,1HK,1HL,1HM,1HN,1HO,1HP,1HQ,1HR/                  EVALSN  17
C              IBM********** REPLACE THE ABOVE CARD WITH                        EVALSN  18
C              DATA INEG/ZD0404040,1HJ,1HK,1HL,1HM,1HN,1HO,1HP,1HQ,1HR/         EVALSN  19
      DATA  INORM    /1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9/                  EVALSN  20
      DATA  IPOS     /1H^,1HA,1HB,1HC,1HD,1HE,1HF,1HG,1HH,1HI/                  EVALSN  21
C              IBM********** REPLACE THE ABOVE CARD WITH                        EVALSN  22
C              DATA IPOS /ZC0404040,1HA,1HB,1HC,1HD,1HE,1HF,1HG,1HH,1HI/        EVALSN  23
      DATA  ISTAR    /1H*/                                                      EVALSN  24
      DATA  NUMBER   /0,1,2,3,4,5,6,7,8,9/                                      EVALSN  25
C                                                                               EVALSN  26
C              CHECK FOR INVALID ENTRY                                          EVALSN  27
      IF (ISGN .EQ. ISTAR .OR. ISGN .EQ. IBLNK .OR. ISGN .EQ. IDASH)            EVALSN  28
     1    GO TO 3000                                                            EVALSN  29
      DO 1000 I = 1,10                                                          EVALSN  30
      IF (ISGN .EQ. IPOS(I) .OR. ISGN .EQ. INORM(I)) GOTO 3010                  EVALSN  31
      IF (ISGN .EQ. INEG(I)) GOTO 3020                                          EVALSN  32
 1000 CONTINUE                                                                  EVALSN  33
C                                                                               EVALSN  34
C              ALL BLANK, UNKNOWN                                               EVALSN  35
 3000 CONTINUE                                                                  EVALSN  36
      NUMB   = IUNDEF                                                           EVALSN  37
      RETURN                                                                    EVALSN  38
 3010 CONTINUE                                                                  EVALSN  39
      NUMB   = NUMB*10 + NUMBER(I)                                              EVALSN  40
      RETURN                                                                    EVALSN  41
 3020 CONTINUE                                                                  EVALSN  42
      NUMB   = -(NUMB*10 + NUMBER(I))                                           EVALSN  43
      RETURN                                                                    EVALSN  44
      END                                                                       EVALSN  45
      SUBROUTINE GETCRD(INFILE,KARD,LIMIT)                                      GETCRD   2
C                                                                               GETCRD   3
C              THIS ROUTINE SIMULATES A        READ(INFILE,5000) KARD           GETCRD   4
C                                     5000 FORMAT(LIMIT(A1))                    GETCRD   5
C                                                                               GETCRD   6
C              AT BKY, THIS IS ONE WAY OF READING LONG                          GETCRD   7
C              FORMATTED RECORDS (NEEDED FOR WX1440 WEATHER                     GETCRD   8
C              TAPES, FOR INSTANCE).  AT OTHER SITES, DIFFERENT                 GETCRD   9
C              METHODS WILL HAVE TO BE USED.  NOTE THAT A FORTRAN               GETCRD  10
C              FORMATTED READ WITH LENGTH > 150 CHARACTERS MAY                  GETCRD  11
C              COMPILE, BUT MAY THEN GIVE INCORRECT RESULTS UPON                GETCRD  12
C              EXECUTION.                                                       GETCRD  13
C                                                                               GETCRD  14
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
C                                                                               GETCRD  16
      DIMENSION  KARD(LIMIT)                                                    GETCRD  17
C                                                                               GETCRD  18
      READ(INFILE,5000,END=3000) (KARD(I),I=1,LIMIT)                            GETCRD  19
 5000 FORMAT(5(99A1))                                                           GETCRD  20
      RETURN                                                                    GETCRD  21
 3000 CONTINUE                                                                  GETCRD  22
      IEOF   = .TRUE.                                                           GETCRD  23
      RETURN                                                                    GETCRD  24
      END                                                                       GETCRD  25
      SUBROUTINE GTEMP(DIF,TMIN,TMAX,TAV,TG)                                    GTEMP    2
      DIMENSION AMON(12),TG(12)                                                 GTEMP    3
      DATA AMON / 15.,46.,74.,95.,135.,166.,196.,227.,258.,288.,                GTEMP    4
     1            319.,349. /                                                   GTEMP    5
      DATA P,PI,PO / 8760.,3.14159265,0.6 /                                     GTEMP    6
C                                                                               GTEMP    7
      BETA   = SQRT(PI/(DIF*P))*10.                                             GTEMP    8
      X      = EXP(-BETA)                                                       GTEMP    9
      X2     = X*X                                                              GTEMP   10
      C      = COS(BETA)                                                        GTEMP   11
      S      = SIN(BETA)                                                        GTEMP   12
      Y      = X2 - 2.*X*C + 1.                                                 GTEMP   13
      Y      = Y / (2.*BETA*BETA)                                               GTEMP   14
      GM     = SQRT(Y)                                                          GTEMP   15
      Z      = (1.-X*(C+S)) / (1.-X*(C-S))                                      GTEMP   16
      PHI    = ATAN(Z)                                                          GTEMP   17
      BO     = (TMAX-TMIN)*0.5                                                  GTEMP   18
      DO 40 I=1,12                                                              GTEMP   19
      THETA  = AMON(I)*24.                                                      GTEMP   20
   40 TG(I)  = TAV - BO*COS(2.*(PI/P)*THETA-PO-PHI)*GM + 460.                   GTEMP   21
      RETURN                                                                    GTEMP   22
      END                                                                       GTEMP   23
      SUBROUTINE HSHIFT(TMYSYR,TMYDNR,TMYDFR,TMYETR,HTOTL,HDN)                  HSHIFT   2
C                                                                               HSHIFT   3
C              SHIFT SOLAR DATA FROM TRUE SOLAR TIME (TST) BINS TO              HSHIFT   4
C              LOCAL STANDARD TIME (LST) BINS.  NOTE THAT ALL VARIABLES         HSHIFT   5
C              THAT END WITH A J REFER TO TST, THOSE THAT END WITH I            HSHIFT   6
C              REFER TO LST.                                                    HSHIFT   7
C                                                                               HSHIFT   8
      COMMON  /TMYME/    IM,KDOM,IH                                             /TMYME/  2
C                                                                               /TMYME/  3
      COMMON  /CONST/    DTOR, PIOVR2, PIOVR4, PIOV12, IBLNK                    /CONST/  2
C                                                                               /CONST/  3
      COMMON  /DAYPAR/   STSD,CTCD,SSS,CSC,HUNDOG,EQTIME,SOLCON,                /DAYPAR/ 2
     1                   TMSHFT,ISHFTH,WNXTHR,WPRVHR                            /DAYPAR/ 3
C                                                                               /DAYPAR/ 4
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
C                                                                               HSHIFT  13
      DIMENSION DTMI(24),AMI(25),AXI(24),SKI(24),                               HSHIFT  14
     1          DTMJ(24),AMJ(25),AXJ(24),SKJ(24)                                HSHIFT  15
      DIMENSION HTOTL(24),HDN(24),HDF(24),TMYDNR(24),TMYSYR(24),                HSHIFT  16
     1          TMYDFR(24),TMYETR(24)                                           HSHIFT  17
C                                                                               HSHIFT  18
C              STATEMENT FUNCTIONS                                              HSHIFT  19
C                                                                               HSHIFT  20
      RBRCKT(AMN,A,AMX) = AMAX1(AMN,AMIN1(AMX,A))                               HSHIFT  21
      IBRCKT(IMN,I,IMX) = MAX0(IMN,MIN0(IMX,I))                                 HSHIFT  22
C                                                                               HSHIFT  23
      DAYDNI = 0.                                                               HSHIFT  24
      DAYDNJ = 0.                                                               HSHIFT  25
      DAYDFI = 0.                                                               HSHIFT  26
      DAYDFJ = 0.                                                               HSHIFT  27
      DAYSYI = 0.                                                               HSHIFT  28
      DAYSYJ = 0.                                                               HSHIFT  29
C              CALCULATE DTMI AND DTMJ, THE FRACTION OF THE HOUR                HSHIFT  30
C              THE SUN IS UP.                                                   HSHIFT  31
      DO 100 IH=1,24                                                            HSHIFT  32
      AXI(IH) = -7.                                                             HSHIFT  33
      AXJ(IH) = -7.                                                             HSHIFT  34
      SKI(IH) = 0.                                                              HSHIFT  35
      SKJ(IH) = 0.                                                              HSHIFT  36
      HDN(IH) = 0.                                                              HSHIFT  37
C              GET THE TST HOUR ANGLE                                           HSHIFT  38
      HRANGT = FLOAT(IH) - 12.                                                  HSHIFT  39
      DTM = HUNDOG - ABS(HRANGT)                                                HSHIFT  40
      IF (HRANGT .GE. 0.)  DTM = DTM + 1.                                       HSHIFT  41
      DTMJ(IH) = RBRCKT(0.,DTM,1.)                                              HSHIFT  42
      IF (TMYETR(IH) .LE. 0.)  DTMJ(IH) = 0.                                    HSHIFT  43
C              GET THE LST HOUR ANGLE                                           HSHIFT  44
      HRANGL = HRANGT + TMSHFT                                                  HSHIFT  45
      DTM = HUNDOG - ABS(HRANGL)                                                HSHIFT  46
      IF (HRANGL .GE. 0.)  DTM = DTM + 1.                                       HSHIFT  47
      DTMI(IH) = RBRCKT(0.,DTM,1.)                                              HSHIFT  48
  100 CONTINUE                                                                  HSHIFT  49
C              CALCULATE AMJ AND AMI, THE AIR MASSES                            HSHIFT  50
      DO 150 IH=1,25                                                            HSHIFT  51
      HRANGT = FLOAT(IH) - 13.                                                  HSHIFT  52
      HRANGL = HRANGT + TMSHFT                                                  HSHIFT  53
C              GET COSINE OF THE ZENITH ANGLE                                   HSHIFT  54
      CZJ = STSD + CTCD*COS(HRANGT*PIOV12)                                      HSHIFT  55
      CZI = STSD + CTCD*COS(HRANGL*PIOV12)                                      HSHIFT  56
      CZJ = AMAX1(0.,CZJ)                                                       HSHIFT  57
      CZI = AMAX1(0.,CZI)                                                       HSHIFT  58
      AMJ(IH) = AMOFCZ(CZJ)                                                     HSHIFT  59
      AMI(IH) = AMOFCZ(CZI)                                                     HSHIFT  60
  150 CONTINUE                                                                  HSHIFT  61
C              GET THE EXTINCTION AND DIFFUSE COEFFICIENTS AT THE               HSHIFT  62
C              TST HOURS                                                        HSHIFT  63
      DO 200 JH=1,24                                                            HSHIFT  64
      IF (DTMJ(JH) .LE. 0.)  GO TO 195                                          HSHIFT  65
      JHM1 = MAX0(JH-1,1)                                                       HSHIFT  66
      GUESS = AXJ(JHM1)                                                         HSHIFT  67
      IF (JH .EQ. 1)  GUESS = -1.                                               HSHIFT  68
      IF ((DTMJ(JH) .LT. 1.) .AND. (JH .LT. 12))  GUESS = -1.                   HSHIFT  69
C              SOLVE FOR THE EXTINCTION COEFFICIENT.  THE EQUATION              HSHIFT  70
C              THAT IS SOLVED IS                                                HSHIFT  71
C              (EXP(AXJ(JH)*AMJ(JH)) + EXP(AXJ(JH)*AMJ(JH+1)))/2. =             HSHIFT  72
C              TMYDNR(JH)/(SOLCON*DTMJ(JH))                                     HSHIFT  73
      AXJ(JH) = SLVEXN(GUESS,AMJ(JH),AMJ(JH+1),2.*TMYDNR(JH)/(SOLCON            HSHIFT  74
     1                 *DTMJ(JH)),.001)                                         HSHIFT  75
C              CALCULATE SKJ                                                    HSHIFT  76
      HRANGT = FLOAT(JH) - 12.                                                  HSHIFT  77
      TMYDFR(JH) = TMYSYR(JH) - TMYDNR(JH)*CZAV(HRANGT)                         HSHIFT  78
      ETMBMR = TMYETR(JH) - (TMYSYR(JH)-TMYDFR(JH))                             HSHIFT  79
      IF (ETMBMR .GT. 0.)  SKJ(JH) = TMYDFR(JH)/ETMBMR                          HSHIFT  80
      SKJ(JH) = AMIN1(SKJ(JH),1.)                                               HSHIFT  81
  195 DAYDNJ = DAYDNJ + TMYDNR(JH)                                              HSHIFT  82
      DAYDFJ = DAYDFJ + TMYDFR(JH)                                              HSHIFT  83
  200 CONTINUE                                                                  HSHIFT  84
C              NOW CALCULATE AXI FROM AXJ AND SKI FROM SKJ BY                   HSHIFT  85
C              INTERPOLATION                                                    HSHIFT  86
      DO 300 IH=1,24                                                            HSHIFT  87
      TST = FLOAT(IH) + TMSHFT                                                  HSHIFT  88
      HORANG = TST - 12.                                                        HSHIFT  89
      JH = INT(TST)                                                             HSHIFT  90
      JH = IBRCKT(1,JH,24)                                                      HSHIFT  91
      JHP1 = IBRCKT(1,JH+1,24)                                                  HSHIFT  92
      IF (DTMI(IH) .LE. 0.)  GO TO 290                                          HSHIFT  93
      AXI(IH) = (TST-FLOAT(JH))*AXJ(JHP1) + (FLOAT(JHP1)-TST)*AXJ(JH)           HSHIFT  94
      SKI(IH) = (TST-FLOAT(JH))*SKJ(JHP1) + (FLOAT(JHP1)-TST)*SKJ(JH)           HSHIFT  95
C              IF LST AND TST BINS ARE THE SAME, SKIP                           HSHIFT  96
      IF ((TST-FLOAT(JH)) .LT. 0.0001)  GO TO 210                               HSHIFT  97
      IF ((FLOAT(JHP1)-TST) .LT. 0.0001)  GO TO 210                             HSHIFT  98
C              NEAR SUNRISE AND SUNSET, THE EXTINCTION COEFFICIENTS             HSHIFT  99
C              MAY NOT BE WELL DEFINED.  FOR SUNRISE, SET AXI EQUAL TO          HSHIFT 100
C              THE NEXT AXJ.  FOR SUNSET, SET AXI EQUAL TO THE PREVIOUS         HSHIFT 101
C              AXJ.                                                             HSHIFT 102
      IF ((DTMI(JH) .LT. 1.) .AND. (HORANG .LT. 0.) .AND.                       HSHIFT 103
     1    (AXJ(JH) .LE. -7.))  AXI(IH) = AXJ(JHP1)                              HSHIFT 104
      IF ((DTMI(JHP1) .LT. 1.) .AND. (HORANG .GE. 0.) .AND.                     HSHIFT 105
     1    (AXJ(JHP1) .LE. -7.))  AXI(IH) = AXJ(JH)                              HSHIFT 106
C              DO SIMILAR THING FOR SKI                                         HSHIFT 107
      IF (SKJ(JH) .LE. 0.)  SKI(IH) = SKJ(JHP1)                                 HSHIFT 108
      IF (SKJ(JHP1) .LE. 0.)  SKI(IH) = SKJ(JH)                                 HSHIFT 109
C              USING AXI, CALCULATE THE BEAM SOLAR IN LST BINS                  HSHIFT 110
  210 EXX = 0.                                                                  HSHIFT 111
      EXXX = 0.                                                                 HSHIFT 112
      AM1 = AMI(IH)                                                             HSHIFT 113
      AM2 = AMI(IH+1)                                                           HSHIFT 114
C              IF NO DIRECT, LEAVE HDN = 0.                                     HSHIFT 115
      IF (AXI(IH) .LE. -7.)  GO TO 280                                          HSHIFT 116
C              FOR BIN EDGE JUST BEFORE SUNRISE, SET THE AIR MASS TO            HSHIFT 117
C              ITS MAXIMUM VALUE.  DITTO FOR THE BIN EDGE JUST PAST             HSHIFT 118
C              SUNSET.                                                          HSHIFT 119
      IF ((DTMI(IH) .LT. 1.) .AND. (HORANG .LT. 0.))  AM1 = 34.                 HSHIFT 120
      IF ((DTMI(IH) .LT. 1.) .AND. (HORANG .GE. 0.))  AM2 = 34.                 HSHIFT 121
C              CHECK FOR EXPONENTIAL OVERFLOW                                   HSHIFT 122
      IF (AXI(IH)*AM1 .GT. -80.)  EXX = EXP(AXI(IH)*AM1)                        HSHIFT 123
      IF (AXI(IH)*AM2 .GT. -80.)  EXXX = EXP(AXI(IH)*AM2)                       HSHIFT 124
      HDN(IH) = 0.5*SOLCON*DTMI(IH)*(EXX+EXXX)                                  HSHIFT 125
C              USE SKI TO CALCULATE THE DIFFUSE SOLAR IN LST BINS               HSHIFT 126
  280 HDF(IH)=SKI(IH)*(SOLCON*DTMI(IH)-HDN(IH))*CZAV(HORANG)                    HSHIFT 127
  290 HDN(IH) = AMAX1(HDN(IH),0.)                                               HSHIFT 128
      HDF(IH) = AMAX1(HDF(IH),0.)                                               HSHIFT 129
      DAYDNI = DAYDNI + HDN(IH)                                                 HSHIFT 130
      DAYDFI = DAYDFI + HDF(IH)                                                 HSHIFT 131
  300 CONTINUE                                                                  HSHIFT 132
      FDN = 1.                                                                  HSHIFT 133
      IF (DAYDNI .GT. 0.0)  FDN = DAYDNJ/DAYDNI                                 HSHIFT 134
      FDF = DAYDFJ/DAYDFI                                                       HSHIFT 135
C              FINALLY MAKE SURE THAT THE BEAM AND TOTAL SOLAR IN LST           HSHIFT 136
C              BINS ARE NORMALIZED TO THE DATA IN TST BINS                      HSHIFT 137
      DO 500 IH=1,24                                                            HSHIFT 138
      HORANG = FLOAT(IH) + TMSHFT - 12.                                         HSHIFT 139
      HDN(IH) = HDN(IH)*FDN                                                     HSHIFT 140
      HDF(IH) = HDF(IH)*FDF                                                     HSHIFT 141
      HTOTL(IH) = HDF(IH) + HDN(IH)*CZAV(HORANG)                                HSHIFT 142
      DAYSYJ = DAYSYJ + TMYSYR(IH)                                              HSHIFT 143
      DAYSYI = DAYSYI + HTOTL(IH)                                               HSHIFT 144
  500 CONTINUE                                                                  HSHIFT 145
      FSY = DAYSYJ/DAYSYI                                                       HSHIFT 146
      DO 510 IH=1,24                                                            HSHIFT 147
      HTOTL(IH) = HTOTL(IH)*FSY                                                 HSHIFT 148
  510 CONTINUE                                                                  HSHIFT 149
      RETURN                                                                    HSHIFT 150
      END                                                                       HSHIFT 151
      FUNCTION HTDGDY(IB,ITEMP,N)                                               HTDGDY   2
C                                                                               HTDGDY   3
C              THIS FUNCTION RETURNS THE HEATING DEGREE DAYS IN                 HTDGDY   4
C              'N' ELEMENTS OF 'IB' USING 'ITEMP' AS A BASE                     HTDGDY   5
C                                                                               HTDGDY   6
      DIMENSION  IB(24,1)                                                       HTDGDY   7
C                                                                               HTDGDY   8
      NDAYS=N/24                                                                HTDGDY   9
      HTDGDY=0.                                                                 HTDGDY  10
      DO 2000 IDAY=1,NDAYS                                                      HTDGDY  11
         IBMAX=IB(1,IDAY)                                                       HTDGDY  12
         IBMIN=IB(1,IDAY)                                                       HTDGDY  13
         DO 1000 I=2,24                                                         HTDGDY  14
            IBMAX=MAX0(IB(I,IDAY),IBMAX)                                        HTDGDY  15
            IBMIN=MIN0(IB(I,IDAY),IBMIN)                                        HTDGDY  16
 1000    CONTINUE                                                               HTDGDY  17
         AV=(FLOAT(IBMAX)+FLOAT(IBMIN))/2.                                      HTDGDY  18
         HTDGDY = HTDGDY + AMAX1(FLOAT(ITEMP)-AV,0.)                            HTDGDY  19
 2000 CONTINUE                                                                  HTDGDY  20
      RETURN                                                                    HTDGDY  21
      END                                                                       HTDGDY  22
      FUNCTION HTDGHR(IB,ITEMP,N)                                               HTDGHR   2
C                                                                               HTDGHR   3
C              THIS FUNCTION RETURNS THE HEATING DEG. HRS./24 IN                HTDGHR   4
C              'N' ELEMENTS OF 'IB' USING 'ITEMP' AS A BASE                     HTDGHR   5
C                                                                               HTDGHR   6
      DIMENSION  IB(N)                                                          HTDGHR   7
C                                                                               HTDGHR   8
      HTDGHR = 0.0                                                              HTDGHR   9
      DO 1000 I = 1,N                                                           HTDGHR  10
      IDIF   = ITEMP - IB(I)                                                    HTDGHR  11
      IF(IDIF .GT. 0) HTDGHR = HTDGHR + FLOAT(IDIF)                             HTDGHR  12
 1000 CONTINUE                                                                  HTDGHR  13
      HTDGHR = HTDGHR/24.0                                                      HTDGHR  14
      RETURN                                                                    HTDGHR  15
      END                                                                       HTDGHR  16
      FUNCTION IROUND(F)                                                        IROUND   2
C                                                                               IROUND   3
C              ROUND A FLOATING POINT NUMBER                                    IROUND   4
C                                                                               IROUND   5
      AF     = F + SIGN(0.5,F)                                                  IROUND   6
      IROUND = INT(AF)                                                          IROUND   7
      RETURN                                                                    IROUND   8
      END                                                                       IROUND   9
      SUBROUTINE MAXDIR(CZ,SOLCON,DIRMAX)                                       MAXDIR   2
C                                                                               MAXDIR   3
C              CALCULATE MAXIMUM DIRECT NORMAL.  THIS IS THE                    MAXDIR   4
C              TMY LIMIT.                                                       MAXDIR   5
C                                                                               MAXDIR   6
      COMMON  /CONST/    DTOR, PIOVR2, PIOVR4, PIOV12, IBLNK                    /CONST/  2
C                                                                               /CONST/  3
      COMMON  /LIMITS/   CZLIM(9)                                               /LIMITS/ 2
C                                                                               /LIMITS/ 3
C                                                                               MAXDIR   9
      ZA     = ACOS(CZ)/DTOR                                                    MAXDIR  10
      IF (ZA .LT. 82.)  GO TO 1000                                              MAXDIR  11
      DO 200 I=1,8                                                              MAXDIR  12
      BL     = 90. - FLOAT(I)                                                   MAXDIR  13
      IF (BL .GT. ZA)  GO TO 200                                                MAXDIR  14
      IUL    = I                                                                MAXDIR  15
      IBL    = IUL + 1                                                          MAXDIR  16
      FZ     = CZLIM(IBL) + (ZA-BL) * (CZLIM(IUL)-CZLIM(IBL))                   MAXDIR  17
      GO TO 2000                                                                MAXDIR  18
  200 CONTINUE                                                                  MAXDIR  19
 1000 FZ     = 1./CZ                                                            MAXDIR  20
 2000 DIRMAX = 0.866*SOLCON*EXP(-0.08*FZ)                                       MAXDIR  21
      RETURN                                                                    MAXDIR  22
      END                                                                       MAXDIR  23
      FUNCTION MAXVEC(IB,N)                                                     MAXVEC   2
C                                                                               MAXVEC   3
C              THIS FUNCTION RETURNS THE MAXIMUM VALUE OF 'N' ELEMENTS          MAXVEC   4
C              OF ARRAY 'IB'                                                    MAXVEC   5
C                                                                               MAXVEC   6
      DIMENSION  IB(N)                                                          MAXVEC   7
C                                                                               MAXVEC   8
      MAXVEC = IB(1)                                                            MAXVEC   9
      DO 1000 I = 2,N                                                           MAXVEC  10
      IF(MAXVEC .LT. IB(I)) MAXVEC = IB(I)                                      MAXVEC  11
 1000 CONTINUE                                                                  MAXVEC  12
      RETURN                                                                    MAXVEC  13
      END                                                                       MAXVEC  14
      FUNCTION MINVEC(IB,N)                                                     MINVEC   2
C                                                                               MINVEC   3
C              THIS FUNCTION RETURNS THE MINIMUM VALUE OF 'N' ELEMENTS          MINVEC   4
C              OF ARRAY 'IB'                                                    MINVEC   5
C                                                                               MINVEC   6
      DIMENSION IB(N)                                                           MINVEC   7
C                                                                               MINVEC   8
      MINVEC = IB(1)                                                            MINVEC   9
      DO 10 I = 2,N                                                             MINVEC  10
      IF(MINVEC .GT. IB(I)) MINVEC = IB(I)                                      MINVEC  11
   10 CONTINUE                                                                  MINVEC  12
      RETURN                                                                    MINVEC  13
      END                                                                       MINVEC  14
      SUBROUTINE MONTHS(NDGB)                                                   MONTHS   2
C                                                                               MONTHS   3
C              COMPUTE MONTHLY AVERAGES OF WEATHER DATA                         MONTHS   4
C                                                                               MONTHS   5
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /GRUNT/    ADMNTM,ADMXTM,AVRH(4),AVSKYD,AVTDDM,AVTDHR(24),        WTH2     1
     1                   AVTDM,AVTDNM,AVTWM,AVWSDM,AVWSM,AVWSNM,HDD(4),         /GRUNT/  3
     2                   CDD(4),HDH(4),CDH(4),HRDY,HRNT,                        /GRUNT/  4
     3                   LENGTH,MNB0,MNB32,MNTDM,MXB32,MXTDM,MXU90,             /GRUNT/  5
     4                   AVDDNR,AVDTHR,DMXDNR,DMXTHR,DMNDNR,DMNTHR,             /GRUNT/  6
     5                   HMXDNR,HMXTHR,AVXDNR,AVXTHR,                           /GRUNT/  7
     6                   NT(250),NTW(250),AVSKY                                 WTH2     2
C                                                                               /GRUNT/  9
      COMMON  /MONTHC/   BEFORE(12),MDAYS(12),MNAMES(12)                        /MONTHC/ 2
      INTEGER            BEFORE                                                 /MONTHC/ 3
C                                                                               /MONTHC/ 4
      COMMON /MYDATA/AVDST(4),DMXST(4),HMXST(4),DST(4),XST(4)                   /MYDATA/ 2
      COMMON  /SKRACH/   SKRACH(5000)                                           /SKRACH/ 2
C              USED BY EDIT, MONTHS, PACKER, STATS                              /SKRACH/ 3
C              ONLY EDIT USES ALL OF SKRACH                                     /SKRACH/ 4
C                                                                               /SKRACH/ 5
      COMMON /SUND/ISUNUP,GUNDOG,HORANG,TDECLN,EQTIME,SOLCON,                   /SUND/   2
     1        ATMEXT,SKYDFF,RAYCOS(3),RDN,BSUN,DECLN,CD,SD,                     /SUND/   3
     2        FSUNUP,CLDCOV,RDNCC,BSCC,DIFSOL,RTOT(4),SKYA                      /SUND/   4
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON /TIM/ IDOY                                                         /TIME/   2
      COMMON  /WEATHD/   IWDID(5),FIRSTW,LRECX,IWSOL,WLONG,WLAT,IWTZN,          /WEATHD/ 2
     1                   CLRNES,TGRND,IWYR,WBT,DBT,PATM,CLDAMT,ISNOW,           /WEATHD/ 3
     2                   IRAIN,IWNDDR,HUMRAT,DENSTY,ENTHAL,SOLRAD,              /WEATHD/ 4
     3                   DIRSOL,ICLDTY,WNDSPD,IRFCP,DPT                         /WEATHD/ 5
      LOGICAL FIRSTW                                                            /WEATHD/ 6
      DIMENSION          CALC(16)                                               /WEATHD/ 7
      EQUIVALENCE        (WBT,CALC(1))                                          /WEATHD/ 8
C                                                                               /WEATHD/ 9
C                                                                               MONTHS  15
      DIMENSION  DEABC(5)                                                       MONTHS  17
      DIMENSION  IWD(24,31,4)                                                   MONTHS  18
      DIMENSION  MNTD(31)                                                       MONTHS  19
      DIMENSION  MXTD(31)                                                       MONTHS  20
C                                                                               MONTHS  21
      EQUIVALENCE (IWD(1,1,1),SKRACH(1868))                                     MONTHS  22
C                                                                               MONTHS  23
      DO 1000 I = 1,250                                                         MONTHS  24
      NTW(I) = 0                                                                MONTHS  25
      NT(I)  = 0                                                                MONTHS  26
 1000 CONTINUE                                                                  MONTHS  27
      DO 1001 I=1,4                                                             MONTHS  28
 1001 AVRH(I) = 0.                                                              MONTHS  29
      MXU90  = 0                                                                MONTHS  30
      MXB32  = 0                                                                MONTHS  31
      MNB32  = 0                                                                MONTHS  32
      MNB0   = 0                                                                MONTHS  33
      AVWSDM = 0.                                                               MONTHS  34
      AVWSNM = 0.                                                               MONTHS  35
      AVTDDM = 0.                                                               MONTHS  36
      AVTDNM = 0.                                                               MONTHS  37
      AVSKYD = 0.                                                               WTH2     5
      AVSKY = 0.                                                                WTH2     6
      HRDY   = 0.                                                               MONTHS  39
      HRNT   = 0.                                                               MONTHS  40
      IFX    = 0                                                                MONTHS  41
      LENGTH = MDAYS(IMNTH)                                                     MONTHS  42
      AVDDNR = 0.                                                               MONTHS  43
      AVDTHR = 0.                                                               MONTHS  44
      DMXDNR = 0.                                                               MONTHS  45
      DMXTHR = 0.                                                               MONTHS  46
      DMNDNR = 1.E20                                                            MONTHS  47
      DMNTHR = 1.E20                                                            MONTHS  48
      HMXDNR = 0.                                                               MONTHS  49
      HMXTHR = 0.                                                               MONTHS  50
      AVXDNR = 0.                                                               MONTHS  51
      AVXTHR = 0.                                                               MONTHS  52
      DO 950 IK=1,4                                                             MONTHS  53
      RTOT(IK)=0.0                                                              MONTHS  54
      AVDST(IK)=0.0                                                             MONTHS  55
      DMXST(IK)=0.0                                                             MONTHS  56
      HMXST(IK)=0.0                                                             MONTHS  57
  950 CONTINUE                                                                  MONTHS  58
 1002 DO 1016 L = 1,LENGTH                                                      MONTHS  59
      DDNR   = 0.                                                               MONTHS  60
      DTHR   = 0.                                                               MONTHS  61
      XDNR   = 0.                                                               MONTHS  62
      XTHR   = 0.                                                               MONTHS  63
      DO 955 IK=1,4                                                             MONTHS  64
      DST(IK)=0.0                                                               MONTHS  65
      XST(IK)=0.0                                                               MONTHS  66
  955 CONTINUE                                                                  MONTHS  67
      IDOY   = IDOY+1                                                           MONTHS  68
      CALL SUN1                                                                 MONTHS  69
      DO 1010 M = 1,24                                                          MONTHS  70
      IDAY   = L                                                                MONTHS  71
      IHOUR  = M                                                                MONTHS  72
      CALL  WDREAD                                                              MONTHS  73
      IF (IEOF) RETURN                                                          MONTHS  74
      IWD(M,L,1) = IROUND(WNDSPD)                                               MONTHS  75
      IWD(M,L,2) = IROUND(DBT)                                                  MONTHS  76
      IWD(M,L,3) = IROUND(WBT)                                                  MONTHS  77
      IWD(M,L,4) = IROUND(CLDAMT)                                               MONTHS  78
      IHP2   = IHOUR + 2                                                        MONTHS  79
      IRH    = IHP2/6                                                           MONTHS  80
      IF (MOD(IHP2,6) .EQ. 0)  AVRH(IRH) = AVRH(IRH) +                          MONTHS  81
     1                           RELHUM(DBT,WBT,PATM)                           MONTHS  82
      CALL WDTSNR                                                               MONTHS  83
      DO 958 IK=1,4                                                             MONTHS  84
      RTOT(IK)=0.0                                                              MONTHS  85
  958 CONTINUE                                                                  MONTHS  86
      IF (ISUNUP .NE. 0) CALL SUN3R                                             MONTHS  87
      DDNR   = DDNR + DIRSOL                                                    MONTHS  88
      DTHR   = DTHR + SOLRAD                                                    MONTHS  89
      IF (DIRSOL .GT. XDNR)  XDNR = DIRSOL                                      MONTHS  90
      IF (SOLRAD .GT. XTHR)  XTHR = SOLRAD                                      MONTHS  91
      DO 960 IK=1,4                                                             MONTHS  92
      DST(IK)=DST(IK)+RTOT(IK)                                                  MONTHS  93
      IF (RTOT(IK) .GT. XST(IK)) XST(IK)=RTOT(IK)                               MONTHS  94
  960 CONTINUE                                                                  MONTHS  95
 1005 CONTINUE                                                                  MONTHS  96
 1010 CONTINUE                                                                  MONTHS  97
      AVDDNR = AVDDNR + DDNR                                                    MONTHS  98
      AVDTHR = AVDTHR + DTHR                                                    MONTHS  99
      IF (DDNR .GT. DMXDNR)  DMXDNR = DDNR                                      MONTHS 100
      IF (DTHR .GT. DMXTHR)  DMXTHR = DTHR                                      MONTHS 101
      IF (DDNR .LT. DMNDNR)  DMNDNR = DDNR                                      MONTHS 102
      IF (DTHR .LT. DMNTHR)  DMNTHR = DTHR                                      MONTHS 103
      IF (XDNR .GT. HMXDNR)  HMXDNR = XDNR                                      MONTHS 104
      IF (XTHR .GT. HMXTHR)  HMXTHR = XTHR                                      MONTHS 105
      AVXDNR = AVXDNR + XDNR                                                    MONTHS 106
      AVXTHR = AVXTHR + XTHR                                                    MONTHS 107
      DO 965 IK=1,4                                                             MONTHS 108
      AVDST(IK)=AVDST(IK)+DST(IK)                                               MONTHS 109
      IF (DST(IK) .GT. DMXST(IK)) DMXST(IK)=DST(IK)                             MONTHS 110
      IF (XST(IK) .GT. HMXST(IK)) HMXST(IK)=XST(IK)                             MONTHS 111
  965 CONTINUE                                                                  MONTHS 112
 1015 CONTINUE                                                                  MONTHS 113
 1016 CONTINUE                                                                  MONTHS 114
      IF(IWYR .LE. 1956 .OR. (IWYR .EQ. 1957 .AND. IMNTH .LT. 6))               MONTHS 115
     1 IFX = 1                                                                  MONTHS 116
      NHRM   = LENGTH*24                                                        MONTHS 117
      AVTDM  = AVG(IWD(1,1,2),NHRM)                                             MONTHS 118
      AVWSM  = AVG(IWD(1,1,1),NHRM)                                             MONTHS 119
      AVTWM  = AVG(IWD(1,1,3),NHRM)                                             MONTHS 120
      AVSKY = AVG(IWD(1,1,4),NHRM)                                              WTH2     7
      AVDDNR = AVDDNR/FLOAT(LENGTH)                                             MONTHS 121
      AVDTHR = AVDTHR/FLOAT(LENGTH)                                             MONTHS 122
      AVXDNR = AVXDNR/FLOAT(LENGTH)                                             MONTHS 123
      AVXTHR = AVXTHR/FLOAT(LENGTH)                                             MONTHS 124
      DO 970 IK=1,4                                                             MONTHS 125
      AVDST(IK)=AVDST(IK)/FLOAT(LENGTH)                                         MONTHS 126
  970 CONTINUE                                                                  MONTHS 127
 1018 CONTINUE                                                                  MONTHS 128
      DO 1040 I = 1,LENGTH                                                      MONTHS 129
C              GET DAILY MAX AND MIN TEMP                                       MONTHS 130
      MM     = MAXVEC(IWD(1,I,2),24)                                            MONTHS 131
      NN     = MINVEC(IWD(1,I,2),24)                                            MONTHS 132
      MXTD(I) = MM                                                              MONTHS 133
      MNTD(I) = NN                                                              MONTHS 134
C              COUNT NUMBER OF DAYS MAX TEMP IS 90 AND ABOVE                    MONTHS 135
      IF(MM .GE. 90) MXU90 = MXU90 + 1                                          MONTHS 136
C              COUNT NUMBER OF DAYS MAX TEMP IS 32 AND BELOW                    MONTHS 137
      IF(MM .LE. 32) MXB32 = MXB32 + 1                                          MONTHS 138
C              COUNT NUMBER OF DAYS MIN TEMP IS 32 AND BELOW                    MONTHS 139
      IF(NN .LE. 32) MNB32 = MNB32 + 1                                          MONTHS 140
C              COUNT NUMBER OF DAYS MIN TEMP IS 0 AND BELOW                     MONTHS 141
      IF(NN .LE. 0) MNB0 = MNB0 + 1                                             MONTHS 142
C              GET SUNRISE AND SUNSET HOURS                                     MONTHS 143
      CALL  SUNPRM(I+NDGB,DEABC)                                                MONTHS 144
      CALL  SUNUD(DEABC,WLAT,WLONG,IWTZN,SRT,SST)                               MONTHS 145
      J1     = 0                                                                MONTHS 146
      J2     = 0                                                                MONTHS 147
      AJ1    = 0.                                                               MONTHS 148
      AJ2    = 0.                                                               MONTHS 149
      DO 1030 J = 1,24                                                          MONTHS 150
C              COUNT NUMBER OF HOURS AT SPECIFIED DRY AND WET BULB TEMPS        MONTHS 151
      DO 1020 K = 1,250                                                         MONTHS 152
      NTP    = K - 100                                                          MONTHS 153
      IF(NTP .EQ. IWD(J,I,3)) NTW(K) = NTW(K) + 1                               MONTHS 154
      IF(NTP .EQ. IWD(J,I,2)) NT(K) = NT(K) + 1                                 MONTHS 155
 1020 CONTINUE                                                                  MONTHS 156
C              GET SUM OF WIND SPEEDS AND TEMPS FOR DAY AND NIGHT               MONTHS 157
      AJ     = FLOAT(J-1) + FLOAT(IFX)*0.5                                      MONTHS 158
      IF(AJ .GE. SRT) GOTO 3000                                                 MONTHS 159
      AVWSNM = AVWSNM + FLOAT(IWD(J,I,1))                                       MONTHS 160
      AVTDNM = AVTDNM + FLOAT(IWD(J,I,2))                                       MONTHS 161
      J1     = J                                                                MONTHS 162
      AJ1    = AJ                                                               MONTHS 163
      GOTO 1030                                                                 MONTHS 164
 3000 CONTINUE                                                                  MONTHS 165
      IF(AJ .GE. SST) GOTO 3010                                                 MONTHS 166
      AVWSDM = AVWSDM + FLOAT(IWD(J,I,1))                                       MONTHS 167
      AVTDDM = AVTDDM + FLOAT(IWD(J,I,2))                                       MONTHS 168
      AVSKYD = AVSKYD + FLOAT(IWD(J,I,4))                                       WTH2     8
      J2     = J                                                                MONTHS 170
      AJ2    = AJ                                                               MONTHS 171
      GOTO 1030                                                                 MONTHS 172
 3010 CONTINUE                                                                  MONTHS 173
      AVWSNM = AVWSNM + FLOAT(IWD(J,I,1))                                       MONTHS 174
      AVTDNM = AVTDNM + FLOAT(IWD(J,I,2))                                       MONTHS 175
 1030 CONTINUE                                                                  MONTHS 176
      HRDY   = HRDY + SST - SRT                                                 MONTHS 177
      HRNT   = HRNT + 24.0 - SST+SRT                                            MONTHS 178
      AVWSNM = AVWSNM - .5*FLOAT(IWD(J1,I,1)) - .5*FLOAT(IWD(J2+1,I,1))         MONTHS 179
     1  + 0.5*(FLOAT(IWD(J1,I,1)) + FLOAT(IWD(J1+1,I,1)))*(SRT-AJ1)             MONTHS 180
     2  + 0.5*(FLOAT(IWD(J2,I,1)) + FLOAT(IWD(J2+1,I,1)))*(AJ2+1.0-SST)         MONTHS 181
      AVTDNM = AVTDNM - .5*FLOAT(IWD(J1,I,2)) - .5*FLOAT(IWD(J2+1,I,2))         MONTHS 182
     1  + 0.5*(FLOAT(IWD(J1,I,2)) + FLOAT(IWD(J1+1,I,2)))*(SRT-AJ1)             MONTHS 183
     2  + 0.5*(FLOAT(IWD(J2,I,2)) + FLOAT(IWD(J2+1,I,2)))*(AJ2+1.0-SST)         MONTHS 184
      AVWSDM = AVWSDM - .5*FLOAT(IWD(J1+1,I,1)) - .5*FLOAT(IWD(J2,I,1))         MONTHS 185
     1  + 0.5*(FLOAT(IWD(J1,I,1)) + FLOAT(IWD(J1+1,I,1)))*(AJ1+1.0-SRT)         MONTHS 186
     2  + 0.5*(FLOAT(IWD(J2,I,1) ) + FLOAT(IWD(J2+1,I,1)))*(SST-AJ2)            MONTHS 187
      AVTDDM = AVTDDM - .5*FLOAT(IWD(J1+1,I,2)) - .5*FLOAT(IWD(J2,I,2))         MONTHS 188
     1  + 0.5*(FLOAT(IWD(J1,I,2)) + FLOAT(IWD(J1+1,I,2)))*(AJ1+1.0-SRT)         MONTHS 189
     2  + 0.5*(FLOAT(IWD(J2,I,2)) + FLOAT(IWD(J2+1,I,2)))*(SST-AJ2)             MONTHS 190
      RL1    = FLOAT(IWD(J1,I,4))                                               MONTHS 191
      RL2    = FLOAT(IWD(J1+1,I,4))                                             MONTHS 192
      RL3    = FLOAT(IWD(J2,I,4))                                               MONTHS 193
      RL4    = FLOAT(IWD(J2+1,I,4))                                             MONTHS 194
      AVSKYD = AVSKYD - 0.5*RL2 - 0.5*RL3 + 0.5*(RL1+RL2)*(AJ1+1.0-SRT)         WTH2     9
     1  + 0.5*(RL3+RL4)*(SST-AJ2)                                               MONTHS 196
 1040 CONTINUE                                                                  MONTHS 197
      MXTDM  = MAXVEC(MXTD(1),LENGTH)                                           MONTHS 198
      MNTDM  = MINVEC(MNTD(1),LENGTH)                                           MONTHS 199
      ADMXTM = AVG(MXTD(1),LENGTH)                                              MONTHS 200
      ADMNTM = AVG(MNTD(1),LENGTH)                                              MONTHS 201
      AVRH(1) = AVRH(1)/FLOAT(LENGTH)                                           MONTHS 202
      AVRH(2) = AVRH(2)/FLOAT(LENGTH)                                           MONTHS 203
      AVRH(3) = AVRH(3)/FLOAT(LENGTH)                                           MONTHS 204
      AVRH(4) = AVRH(4)/FLOAT(LENGTH)                                           MONTHS 205
      AVWSNM = AVWSNM - FLOAT(IWD(1,1,1))*.5+FLOAT(IWD(24,LENGTH,1))*.5         MONTHS 206
      AVTDNM = AVTDNM - FLOAT(IWD(1,1,2))*.5+FLOAT(IWD(24,LENGTH,2))*.5         MONTHS 207
      AVWSNM = AVWSNM/HRNT                                                      MONTHS 208
      AVTDNM = AVTDNM/HRNT                                                      MONTHS 209
      AVWSDM = AVWSDM/HRDY                                                      MONTHS 210
      AVTDDM = AVTDDM/HRDY                                                      MONTHS 211
      AVSKYD = AVSKYD/HRDY                                                      WTH2    10
C              GET   DEGREEE  DAYS                                              MONTHS 213
      HDD(1) = HTDGDY(IWD(1,1,2),65,NHRM)                                       MONTHS 214
                                                                                MONTHS 215
      HDD(2) = HTDGDY(IWD(1,1,2),60,NHRM)                                       MONTHS 216
      HDD(3) = HTDGDY(IWD(1,1,2),55,NHRM)                                       MONTHS 217
      HDD(4) = HTDGDY(IWD(1,1,2),50,NHRM)                                       MONTHS 218
      CDD(1) = CLDGDY(IWD(1,1,2),80,NHRM)                                       MONTHS 219
      CDD(2) = CLDGDY(IWD(1,1,2),75,NHRM)                                       MONTHS 220
      CDD(3) = CLDGDY(IWD(1,1,2),70,NHRM)                                       MONTHS 221
      CDD(4) = CLDGDY(IWD(1,1,2),65,NHRM)                                       MONTHS 222
      HDH(1) = HTDGHR(IWD(1,1,2),65,NHRM)                                       MONTHS 223
      HDH(2) = HTDGHR(IWD(1,1,2),60,NHRM)                                       MONTHS 224
      HDH(3) = HTDGHR(IWD(1,1,2),55,NHRM)                                       MONTHS 225
      HDH(4) = HTDGHR(IWD(1,1,2),50,NHRM)                                       MONTHS 226
      CDH(1) = CLDGHR(IWD(1,1,2),80,NHRM)                                       MONTHS 227
      CDH(2) = CLDGHR(IWD(1,1,2),75,NHRM)                                       MONTHS 228
      CDH(3) = CLDGHR(IWD(1,1,2),70,NHRM)                                       MONTHS 229
      CDH(4) = CLDGHR(IWD(1,1,2),65,NHRM)                                       MONTHS 230
      DO 1060 I = 1,24                                                          MONTHS 231
      AVTDHR(I) = 0.0                                                           MONTHS 232
      DO 1050 J = 1,LENGTH                                                      MONTHS 233
      AVTDHR(I) = AVTDHR(I) + FLOAT((IWD(I,J,2)))                               MONTHS 234
 1050 CONTINUE                                                                  MONTHS 235
      AVTDHR(I) = AVTDHR(I)/FLOAT(LENGTH)                                       MONTHS 236
 1060 CONTINUE                                                                  MONTHS 237
      RETURN                                                                    MONTHS 238
      END                                                                       MONTHS 239
      FUNCTION NUMB(ISTR,ISTART,IEND)                                           NUMB     2
C                                                                               NUMB     3
C              THIS ROUTINE CONVERTS THE 'ISTART'TH THROUGH 'IEND'TH            NUMB     4
C              ELEMENT OF 'ISTR' INTO AN INTEGER                                NUMB     5
C                                                                               NUMB     6
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
      DIMENSION  ISTR(2000)                                                     NUMB     9
      INTEGER    DOLLAR                                                         NUMB    10
      INTEGER  IDIGIT(10)                                                       NUMB    11
      DATA  IDIGIT/1H0, 1H1, 1H2, 1H3, 1H4, 1H5, 1H6, 1H7, 1H8, 1H9/            NUMB    12
C                                                                               NUMB    13
      DATA  IBLNK    /1H /                                                      NUMB    14
      DATA  IMINUS   /1H-/                                                      NUMB    15
      DATA  IPLUS    /1H+/                                                      NUMB    16
      DATA DOLLAR/ 1H$ /                                                        NUMB    17
 7000 FORMAT(33H1ERROR ENCOUNTERED - ROUTINE NUMB /                             NUMB    18
     1       35H NON-NUMERIC VALUE IN NUMERIC FIELD /                           NUMB    19
     2       11H0POSITIONS  ,I5, 6H THRU  ,I5,10H IN STRING /                   NUMB    20
     3       1X,130A1)                                                          NUMB    21
C                                                                               NUMB    22
      NUMB   = 0                                                                NUMB    23
      ISIGN  = 1                                                                NUMB    24
      IE     = IEND                                                             NUMB    25
      DO 1000 I = ISTART,IE                                                     NUMB    26
      IF (ISTR(I) .NE. IBLNK) GOTO 3000                                         NUMB    27
 1000 CONTINUE                                                                  NUMB    28
      RETURN                                                                    NUMB    29
 3000 CONTINUE                                                                  NUMB    30
      IF (ISTR(I) .EQ. IMINUS) ISIGN = -1                                       NUMB    31
      IF (ISTR(I) .EQ. IPLUS .OR. ISTR(I) .EQ. IMINUS) I = I + 1                NUMB    32
      IS     = I                                                                NUMB    33
      IF (IS .GT. IE)  NUMB = IUNDEF                                            NUMB    34
      IF (IS .GT. IE)  RETURN                                                   NUMB    35
      DO 1010 I = IS,IE                                                         NUMB    36
      IC = ISTR(I)                                                              NUMB    37
        DO 1100  J = 1 , 10                                                     NUMB    38
        IF( IC .EQ. IDIGIT(J) )  GOTO 1200                                      NUMB    39
1100    CONTINUE                                                                NUMB    40
      GOTO 3010                                                                 NUMB    41
1200  NUMB = NUMB*10 + J-1                                                      NUMB    42
 1010 CONTINUE                                                                  NUMB    43
      NUMB   = ISIGN*NUMB                                                       NUMB    44
      RETURN                                                                    NUMB    45
 3010 CONTINUE                                                                  NUMB    46
      IF (IC .EQ. IMINUS)  NUMB = IUNDEF                                        NUMB    47
      IF (IC .EQ. IMINUS)  RETURN                                               NUMB    48
      WRITE(OUTFIL,7000)ISTART,IE,(ISTR(I),I=ISTART,IEND)                       NUMB    49
      JUMP   = IEND - ISTART - 1                                                NUMB    50
      WRITE(OUTFIL,7001) (IBLNK,I=1,ISTART), DOLLAR,                            NUMB    51
     1            (IBLNK,I=1,JUMP), DOLLAR                                      NUMB    52
 7001 FORMAT(130A1)                                                             NUMB    53
       CALL  ABT                                                                NUMB    54
      RETURN                                                                    NUMB    55
      END                                                                       NUMB    56
      FUNCTION NUMB2(ISTR,ISTART,IEND,LEN)                                      -055    66
C                                                                               NUMB2    3
C              SAME AS NUMB, BUT RETURNS IUNDEF IF THE STRING IS                NUMB2    4
C              ALL BLANKS                                                       NUMB2    5
C                                                                               NUMB2    6
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
      DIMENSION ISTR(LEN)                                                       -055    67
      INTEGER    IDIGIT(10)                                                     NUMB2   12
      DATA  IDIGIT/1H0, 1H1, 1H2, 1H3, 1H4, 1H5, 1H6, 1H7, 1H8, 1H9/            NUMB2   13
C                                                                               NUMB2   14
      DATA  IBLNK    /1H /                                                      NUMB2   15
      DATA  IMINUS   /1H-/                                                      NUMB2   16
      DATA  IPLUS    /1H+/                                                      NUMB2   17
      DATA ISLASH/ 1H/ /                                                        NUMB2   19
C                                                                               NUMB2   20
      NUMB2   = 0                                                               NUMB2   26
      ISIGN  = 1                                                                NUMB2   27
      IE     = IEND                                                             NUMB2   28
      DO 1000 I = ISTART,IE                                                     NUMB2   29
      IF ((ISTR(I) .NE. IBLNK) .AND. (ISTR(I) .NE. ISLASH)) GO TO 3000          WTH1     3
 1000 CONTINUE                                                                  NUMB2   31
      NUMB2 = IUNDEF                                                            NUMB2   32
      RETURN                                                                    NUMB2   33
 3000 CONTINUE                                                                  NUMB2   34
      IF (ISTR(I) .EQ. IMINUS) ISIGN = -1                                       NUMB2   35
      IF (ISTR(I) .EQ. IPLUS .OR. ISTR(I) .EQ. IMINUS) I = I + 1                NUMB2   36
      IS     = I                                                                NUMB2   37
      IF (IS .GT. IE)  NUMB2 = IUNDEF                                           NUMB2   38
      IF (IS .GT. IE)  RETURN                                                   NUMB2   39
      DO 1010 I = IS,IE                                                         NUMB2   40
      IC = ISTR(I)                                                              NUMB2   41
        DO 1100  J=1,10                                                         NUMB2   42
        IF (IC .EQ. IDIGIT(J))  GO TO 1200                                      NUMB2   43
 1100   CONTINUE                                                                NUMB2   44
      GO TO 3010                                                                NUMB2   45
 1200 NUMB2 = NUMB2*10 + J-1                                                    NUMB2   46
 1010 CONTINUE                                                                  NUMB2   47
      NUMB2   = ISIGN*NUMB2                                                     NUMB2   48
      RETURN                                                                    NUMB2   49
 3010 CONTINUE                                                                  NUMB2   50
      IF (IC .EQ. IMINUS)  RETURN                                               NUMB2   51
      IF (IC .EQ. ISLASH)  RETURN                                               NUMB2   52
C              ILLEGAL CHARACTER; PRINT ERROR MESSAGE AND ABORT                 -055    68
      WRITE (OUTFIL,9001) IC,(ISTR(J),J=1,LEN)                                  -055    69
 9001 FORMAT(//' ERROR ENCOUNTERED - ROUTINE NUMB2'/                            -055    70
     1       ' NON-NUMERIC VALUE IN NUMERIC FIELD'/                             -055    71
     2       ' THE ILLEGAL CHARACTER IS: 'A1/                                   -055    72
     3       ' THE DATA RECORD IS:'/                                            -055    73
     4       1H ,80A1)                                                          -055    74
      CALL  ABT                                                                 NUMB2   56
      RETURN                                                                    NUMB2   57
      END                                                                       NUMB2   58
      SUBROUTINE OTHER                                                          OTHER    2
C                                                                               OTHER    3
C              INSERT YOUR ROUTINE TO DECODE SPECIAL TAPE FORMAT                OTHER    4
C                                                                               OTHER    5
      COMMON  /LOCALD/   STALAT,STALON,SSTALA,CSTALA,TSTALA,HRSLON              /LOCALD/ 2
C                                                                               /LOCALD/ 3
      COMMON  /CONST/    DTOR, PIOVR2, PIOVR4, PIOV12, IBLNK                    /CONST/  2
C                                                                               /CONST/  3
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /MONTHC/   BEFORE(12),MDAYS(12),MNAMES(12)                        /MONTHC/ 2
      INTEGER            BEFORE                                                 /MONTHC/ 3
C                                                                               /MONTHC/ 4
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /PACKEC/   NTZ,ISTAT,XLAT,XLONG,                                  /PACKEC/ 2
     1                   IYR,INTINT,DDBT,IBEGH,NBS,IDOYSH(5),ISSHFT(5),         /PACKEC/ 3
     2                   ISHFT,ALT,SKYCOV(12),ILOC                              073a     1
C                                                                               /PACKEC/ 5
      COMMON  /PARAMS/   STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 2
      LOGICAL            STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 3
C                                                                               /PARAMS/ 4
      COMMON  /PCKINT/   KYR,KSTAT,CN(12),TG(12),KTIM,KARD(500),KYRL            -089     1
C                                                                               /PCKINT/ 3
      COMMON  /RAWDAT/   IDRY(24),IWET(24),IDEW(24),IPRESS(24),                 /RAWDAT/ 2
     1                   IWNDSP(24),ICLAMT(24),ISOL(24),IDN(24),                /RAWDAT/ 3
     2                   IWNDIR(24),ICLTY(24),IRN(24),ISN(24),                  CD144S   1
     3                   IOPQCA(24),IRELH(24),ICLTY1(24)                        CD144S   2
      DIMENSION IRAW(24,14)                                                     CD144S   3
      EQUIVALENCE        (IDRY(1),IRAW(1,1))                                    /RAWDAT/ 6
      COMMON /LSTIME/   LSTHRS(24)                                              /RAWDAT/ 7
C                                                                               /RAWDAT/ 8
      COMMON  /REPORC/   IBEGM,IENDM                                            /REPORC/ 2
C                                                                               /REPORC/ 3
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
C                                                                               OTHER   14
      DIMENSION DEABC(5)                                                        MSWTH    5
C              DAY OF YEAR                                                      MSWTH    6
      IDOY = BEFORE(IMNTH) + IDAY                                               MSWTH    7
C              GET SUN PARAMETERS                                               MSWTH    8
      CALL SUNPRM(IDOY,DEABC)                                                   MSWTH    9
C              SOLAR CONSTANT                                                   MSWTH   10
      SOLCON = 435.2*(1.+0.033*COS(DTOR*360.*FLOAT(IDOY)/365.))                 MSWTH   11
C              LOOP OVER HOURS IN THE DAY                                       MSWTH   12
      DO 1000 IH=1,24                                                           MSWTH   13
      DIRN = 0.                                                                 MSWTH   14
      DIF = 0.                                                                  MSWTH   15
      DIRH = 0.                                                                 MSWTH   16
C              READ IN WEATHER DATA                                             MSWTH   17
      READ (INWTH,9001) IHR,ID,IM,IWYR,TDRY,RELH,PRESMB,SOLH,                   MSWTH   18
     1                  WS,WD,SKYFR                                             MSWTH   19
 9001 FORMAT(3I2,I4,F10.1,F10.2,F10.1,F10.0,F10.1,F10.0,F10.1)                  MSWTH   20
C              CONVERT DRYBULB FROM CENTIGRADE TO FAHRENHEIT                    MSWTH   21
      TDRYF = 1.8*TDRY + 32.                                                    MSWTH   22
C              CALCULATE WETBULB AND DEWPOINT                                   MSWTH   23
C              SATURATED VAPOR PRESSURE                                         MSWTH   24
      PS = PPWVMS(TDRYF)                                                        MSWTH   25
C              PARTIAL PRESSURE                                                 MSWTH   26
      PW = RELH*PS                                                              MSWTH   27
C              CONVERT PRESSURE FROM MILLIBARS TO INCHES OF HG                  MSWTH   28
      PRESHG = .02953*PRESMB                                                    MSWTH   29
C              HUMIDITY RATIO                                                   MSWTH   30
      HUMRAT = 0.622*PW/(PRESHG-PW)                                             MSWTH   31
C              SPECIFIC ENTHALPY                                                MSWTH   32
      ENTH = 0.24*TDRYF + (1061.+0.444*TDRYF)*HUMRAT                            MSWTH   33
      TWETF = WBF(ENTH,PRESHG)                                                  MSWTH   34
      Y = LOG(PW)                                                               MSWTH   35
      IF (PW .LE. 0.1836) THEN                                                  MSWTH   36
          TDEWF = 71.98 + 24.873*Y + 0.8927*Y*Y                                 MSWTH   37
      ELSE                                                                      MSWTH   38
          TDEWF = 79.047 + 30.579*Y + 1.8893*Y*Y                                MSWTH   39
      END IF                                                                    MSWTH   40
C              CONVERT WINDSPEED FROM M/S TO KNOTS                              MSWTH   41
      WSKNOT = 1.9438*WS                                                        MSWTH   42
C              TOTAL HORIZONTAL SOLAR; CONVERT FROM W/M**2 TO                   MSWTH   43
C              BTU/(FT**2)(HR)                                                  MSWTH   44
      SOLHOR = .31721*SOLH                                                      MSWTH   45
C              GET HOUR ANGLE AT UPPER AND LOWER HOUR BIN EDGE                  MSWTH   46
C              THIS IS LOCAL TIME!                                              MSWTH   47
      UL     = FLOAT(IH) - 12. + FLOAT(NTZ) + DEABC(2) - XLONG/15.              MSWTH   48
      BL     = UL - 1.                                                          MSWTH   49
C              SUNRISE AND SUNSET HOUR ANGLES                                   MSWTH   50
      SSHA   = ACOS(-TAN(STALAT)*TAN(DEABC(1)))/PIOV12                          MSWTH   51
      SRHA   = -SSHA                                                            MSWTH   52
C              RESET BIN BOUNDARIES TO ALLOW FOR SUNRISE AND SET                MSWTH   53
      IF ((UL .LE. SRHA) .OR. (BL .GE. SSHA)) GO TO 500                         MSWTH   54
        IF (SRHA .GT. BL)  BL = SRHA                                            MSWTH   55
        IF (SSHA .LT. UL)  UL = SSHA                                            MSWTH   56
        IF ((UL-BL) .LT. 0.02)  GO TO 500                                       MSWTH   57
        IF (SOLHOR .EQ. 0) GO TO 500                                            MSWTH   58
        A = SIN(DEABC(1))*SIN(STALAT)                                           MSWTH   59
        B = COS(DEABC(1))*COS(STALAT)                                           MSWTH   60
C              INTEGRATE SOLAR Z DIREC. COSINE OVER BIN                         MSWTH   61
        COSZIN = A*(UL-BL) + B*(SIN(PIOV12*UL)-SIN(PIOV12*BL))/PIOV12           MSWTH   62
C              AVERAGE COSINE OF THE SOLAR ZENITH ANGLE FOR THE HOUR            MSWTH   63
        COSZAV = COSZIN/(UL-BL)                                                 MSWTH   64
C              EXTRATERRESTRIAL SOLAR HORIZONTAL                                MSWTH   65
        SOLEXH = SOLCON*COSZIN                                                  MSWTH   66
C              K sub T IS THE RATIO OF TERRESTRIAL TO EXTRATERRESTRIAL SOLAR    MSWTH   67
        RKT = AMIN1(SOLHOR/SOLEXH,0.9)                                          MSWTH   68
C              GET DIFFUSE COMPONENT FROM ERBS, KLEIN, AND DUFFIE               MSWTH   69
C              CORRELATION                                                      MSWTH   70
        IF (RKT .LE. 0.22)  DIF = SOLHOR*(1.-0.09*RKT)                          MSWTH   71
        RKT2 = RKT*RKT                                                          MSWTH   72
        IF ((RKT .GT. 0.22) .AND. (RKT .LE. 0.8))  DIF = SOLHOR*                MSWTH   73
     1   (.9511-.1604*RKT+4.388*RKT2-16.638*RKT*RKT2+12.336*RKT2*RKT2)          MSWTH   74
        IF (RKT .GT. 0.8)  DIF = 0.165*SOLHOR                                   MSWTH   75
C              DIRECT HORIZONTAL                                                MSWTH   76
        DIRH = AMAX1(0.,SOLHOR-DIF)                                             MSWTH   77
C              DIRECT NORMAL                                                    MSWTH   78
        DIRN = DIRH/COSZAV                                                      MSWTH   79
C              CHECK FOR MAXIMUM DIRECT NORMAL                                  MSWTH   80
        CALL MAXDIR(COSZAV,SOLCON,DIRMAX)                                       MSWTH   81
        DIRN = AMIN1(DIRN,DIRMAX)                                               MSWTH   82
  500 CONTINUE                                                                  MSWTH   83
C              FILL THE DATA ARRAYS                                             MSWTH   84
      IDRY(IH) = IROUND(TDRYF)                                                  MSWTH   85
      IWET(IH) = MIN0(IDRY(IH),IROUND(TWETF))                                   MSWTH   86
      IDEW(IH) = MIN0(IWET(IH),IROUND(TDEWF))                                   MSWTH   87
      IPRESS(IH) = IROUND(100.*PRESHG)                                          MSWTH   88
      IWNDSP(IH) = IROUND(WSKNOT)                                               MSWTH   89
      IWNDIR(IH) = IROUND(.0444444*WD)                                          MSWTH   90
      IF (IWNDIR(IH) .EQ. 16) IWNDIR(IH) = 0                                    MSWTH   91
      IF (SKYFR.EQ.-999.) THEN                                                  MSWTH   92
        ICLAMT(IH) = 5                                                          MSWTH   93
      ELSE                                                                      MSWTH   94
        ICLAMT(IH) = IROUND(10.*SKYFR)                                          MSWTH   95
      ENDIF                                                                     MSWTH   96
      ISOL(IH) = IROUND(SOLHOR)                                                 MSWTH   97
      IDN(IH) = IROUND(DIRN)                                                    MSWTH   98
      ICLTY(IH) = 2                                                             MSWTH   99
      ICLTY1(IH) = 2                                                            MSWTH  100
      IRN(IH) = 0                                                               MSWTH  101
      ISN(IH) = 0                                                               MSWTH  102
 1000 CONTINUE                                                                  MSWTH  103
      RETURN                                                                    OTHER   15
      END                                                                       OTHER   16
      SUBROUTINE PACKER                                                         PACKER   2
C                                                                               PACKER   3
C              PACKER READS DATA FROM TRY, TMY, CTZ OR WX1440 TAPES AND         PACKER   4
C              CREATES A PACKED BINARY FILE READABLE BY DOE-1 OR -2. IF         PACKER   5
C              DESIRED, ALSO CREATES A FILE TAPE9 FOR PLOTTING PURPOSES.        PACKER   6
C                                                                               PACKER   7
      COMMON  /CONST/    DTOR, PIOVR2, PIOVR4, PIOV12, IBLNK                    /CONST/  2
C                                                                               /CONST/  3
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /FILTYP/   IFTYP,IWSZ,ITPTYP                                      /FILTYP/ 2
C                                                                               /FILTYP/ 3
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /IDAT/     IDAT(1536)                                             /IDAT/   2
C                                                                               /IDAT/   3
      COMMON  /LOCALD/   STALAT,STALON,SSTALA,CSTALA,TSTALA,HRSLON              /LOCALD/ 2
C                                                                               /LOCALD/ 3
      COMMON  /MONTHC/   BEFORE(12),MDAYS(12),MNAMES(12)                        /MONTHC/ 2
      INTEGER            BEFORE                                                 /MONTHC/ 3
C                                                                               /MONTHC/ 4
      COMMON  /PACKEC/   NTZ,ISTAT,XLAT,XLONG,                                  /PACKEC/ 2
     1                   IYR,INTINT,DDBT,IBEGH,NBS,IDOYSH(5),ISSHFT(5),         /PACKEC/ 3
     2                   ISHFT,ALT,SKYCOV(12),ILOC                              073a     1
C                                                                               /PACKEC/ 5
      COMMON  /PARAMS/   STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 2
      LOGICAL            STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 3
C                                                                               /PARAMS/ 4
      COMMON  /PCKINT/   KYR,KSTAT,CN(12),TG(12),KTIM,KARD(500),KYRL            -089     1
C                                                                               /PCKINT/ 3
      COMMON  /RAWDAT/   IDRY(24),IWET(24),IDEW(24),IPRESS(24),                 /RAWDAT/ 2
     1                   IWNDSP(24),ICLAMT(24),ISOL(24),IDN(24),                /RAWDAT/ 3
     2                   IWNDIR(24),ICLTY(24),IRN(24),ISN(24),                  CD144S   1
     3                   IOPQCA(24),IRELH(24),ICLTY1(24)                        CD144S   2
      DIMENSION IRAW(24,14)                                                     CD144S   3
      EQUIVALENCE        (IDRY(1),IRAW(1,1))                                    /RAWDAT/ 6
      COMMON /LSTIME/   LSTHRS(24)                                              /RAWDAT/ 7
C                                                                               /RAWDAT/ 8
      COMMON  /SKIPPY/   FIRSTS,NCALL                                           /SKIPPY/ 2
      LOGICAL            FIRSTS                                                 /SKIPPY/ 3
C                                                                               /SKIPPY/ 4
      COMMON  /SKRACH/   SKRACH(5000)                                           /SKRACH/ 2
C              USED BY EDIT, MONTHS, PACKER, STATS                              /SKRACH/ 3
C              ONLY EDIT USES ALL OF SKRACH                                     /SKRACH/ 4
C                                                                               /SKRACH/ 5
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
      COMMON  /WEATHD/   IWDID(5),FIRSTW,LRECX,IWSOL,WLONG,WLAT,IWTZN,          /WEATHD/ 2
     1                   CLRNES,TGRND,IWYR,WBT,DBT,PATM,CLDAMT,ISNOW,           /WEATHD/ 3
     2                   IRAIN,IWNDDR,HUMRAT,DENSTY,ENTHAL,SOLRAD,              /WEATHD/ 4
     3                   DIRSOL,ICLDTY,WNDSPD,IRFCP,DPT                         /WEATHD/ 5
      LOGICAL FIRSTW                                                            /WEATHD/ 6
      DIMENSION          CALC(16)                                               /WEATHD/ 7
      EQUIVALENCE        (WBT,CALC(1))                                          /WEATHD/ 8
C                                                                               /WEATHD/ 9
C                                                                               PACKER  24
      DIMENSION  IRAW3(72,14),IRAWP(14)                                         CD144S  51
      DIMENSION  ITAPE(2), IBITS(2), IFILE(2)                                   PACKER  26
      DIMENSION NAMS(4,17)                                                      073a     2
      DIMENSION DBTAV(12)                                                       PACKER  28
      DIMENSION  NAMES(3,14)                                                    CD144S  52
      DIMENSION NAMETT(2,17)                                                    -089     3
      DIMENSION  NHRP(14),NHRE(14),NHRE2(14)                                    CD144S  53
      DIMENSION  IWDSZ(2,2), IFLNAM(2,3)                                        PACKER  32
      DIMENSION  WD(10),IWD(4)                                                  CD144S  54
      DIMENSION IDAT30(1536), IDAT60(768), IDATO(1488)                          PACKER  34
      DIMENSION MSHFTN(5), IDSHFT(5)                                            PACKER  35
      DIMENSION LSTHRX(24)                                                      PACKER  36
      REAL DEABC(5)                                                             CD144S  55
C                                                                               PACKER  37
      EQUIVALENCE (SKRACH(1),IRAW3(1,1))                                        PACKER  38
      EQUIVALENCE (IDAT(1),IDAT30(1)),(IDAT(1),IDAT60(1)),                      PACKER  39
     1            (IDAT(1),IDATO(1))                                            PACKER  40
C                                                                               PACKER  41
      LOGICAL  FIRSTE, FIRSTD, RHCALC                                           CD144S  56
      CHARACTER JCITY*22,JSTATE*2,JNS*1,JEW*1                                   -089     4
      INTEGER JTZ,JLATDG,JLATMN,JLONGD,JLONGM,JELEV                             -089     5
C                                                                               PACKER  43
      EXTERNAL  CTZDCD                                                          PACKER  44
      EXTERNAL  TRYDCD                                                          PACKER  45
      EXTERNAL  WXDCD                                                           PACKER  46
      EXTERNAL  TMYDCD                                                          PACKER  47
C                                                                               PACKER  48
      DATA  NAMS /                                                              PACKER  49
     1            4HTAPE,4H TYP,4HE   ,4H    ,                                  PACKER  50
     2            4HSTAT,4HION ,4HNUMB,4HER  ,                                  PACKER  51
     3            4HYEAR,4H    ,4H    ,4H    ,                                  PACKER  52
     4            4HTIME,4H ZON,4HE   ,4H    ,                                  PACKER  53
     5            4HLATI,4HTUDE,4H    ,4H    ,                                  PACKER  54
     6            4HLONG,4HITUD,4HE   ,4H    ,                                  PACKER  55
     7            4HWORD,4H SIZ,4HE   ,4H    ,                                  PACKER  56
     8            4HPLOT,4H FLA,4HG   ,4H    ,                                  PACKER  57
     9            4HINTE,4HRP I,4HNTER,4HVAL ,                                  PACKER  58
     1            4HMAX ,4HTEMP,4HCHAN,4HGE  ,                                  PACKER  59
     2            4HCLEA,4HRNES,4HS NU,4HMBER,                                  PACKER  60
     3            4HGROU,4HND T,4HEMP ,4H    ,                                  PACKER  61
     4            4HFILE,4H TYP,4HE   ,4H    ,                                  PACKER  62
     5            4HSOIL,4H DIF,4HFUSI,4HVITY,                                  PACKER  63
     6            4HALTI,4HTUDE,4H    ,4H    ,                                  WTH2    13
     7            4HSKY ,4HCOVE,4HR   ,4H    ,                                  073a     4
     8            4HCLD ,4HCOV ,4HMOD ,4HCODE /                                 073a     5
      DATA IWDSZ /4H60-B,2HIT,4H30-B,2HIT/                                      PACKER  65
      DATA IFLNAM /4HOLD ,2H  ,4HNORM,2HAL,4HSOLA,2HR /                         PACKER  66
      DATA IEND/4HEND /                                                         PACKER  67
      DATA  NAMES    /4HDRYB,4HULB ,4HTEMP,                                     PACKER  68
     1                4HWETB,4HULB ,4HTEMP,                                     PACKER  69
     2                4HDEWP,4HOINT,4H TMP,                                     PACKER  70
     3                4HPRES,4HSURE,4H    ,                                     PACKER  71
     4                4HWIND,4H SPE,4HED   ,                                    PACKER  72
     5                4HCLOU,4HD AM,4HOUNT,                                     PACKER  73
     6                4HHOR ,4HRAD ,4H    ,                                     PACKER  74
     7                4HDIR ,4HNORM,4H SOL,                                     PACKER  75
     8                4HWIND,4H DIR,4H    ,                                     PACKER  76
     9                4HCLOU,4HD TY,4HPE  ,                                     PACKER  77
     1                4HRAIN,4H FLA,4HG   ,                                     PACKER  78
     2                4HSNOW,4H FLA,4HG   ,                                     CD144S  57
     3                4HOPQ ,4HCLD ,4HAMNT,                                     CD144S  58
     4                4HREL ,4HHUMI,4HDITY /                                    CD144S  59
      DATA NAMETT  /  4HZZZZ,2HZZ,4HOTHE,2HR ,                                  PACKER  80
     1                4HTRY ,2H  ,4H1440,2H  ,                                  PACKER  81
     2                4H1440,2H-3,4HCTZ ,2H  ,                                  PACKER  82
     3                4HTMY ,2H  ,4HSOLM,2HET,                                  PACKER  83
     4                4HWYEC,2H  ,4HTRYK,2HS ,                                  PACKER  84
     5                4HCD14,2H4 ,4HCD14,2H4S,                                  CD144S  60
     6                4HTD96,2H85,4HCANA,2HDA,4HWYEC,2H2 ,4HTRYS,2HLM,          -089     6
     7                4HTMY2,2H                                                 -089     7
     .                /                                                         PACKER  87
C                                                                               PACKER  88
 5000 FORMAT(5A4)                                                               PACKER  89
 5001 FORMAT(A4,A2,3I6,2F6.1,2(A4,A2),I6,2F6.1,F6.0,I6)                         073a     7
 5002 FORMAT(12F6.1)                                                            PACKER  91
 5003 FORMAT(I6,6X,5(A3,I3),5I6)                                                PACKER  92
 5004 FORMAT(1X,I5,1X,A22,1X,A2,1X,I3,1X,A1,1X,I2,1X,I2,1X,A1,1X,I3,            -089     8
     1       1X,I2,2X,I4)                                                       -089     9
 6000 FORMAT(11X,12HSTATION NAME,10X,5A4)                                       PACKER  93
 6001 FORMAT(////1H ,14H--- ERROR --- ,20HNOT ENOUGH INPUT FOR,                 PACKER  94
     1       39H RUN TYPE PACK.  PROCESSING TERMINATED.)                        PACKER  95
 6002 FORMAT(////1H ,36HPROCESSING ENDED DUE TO INPUT ERRORS)                   PACKER  96
 6004 FORMAT(1H1)                                                               PACKER  97
 6005 FORMAT(////1X,32HDATES AND DIRECTION OF SOLAR BIN,                        PACKER  98
     1       22H SHIFTS FOR WYEC TAPES / 1X,6(10H----------),2H-- //)           PACKER  99
 6006 FORMAT(11X,A3,1X,I2,I4)                                                   PACKER 100
 8000 FORMAT(1H ,15H--- WARNING ---,10X,A4,I2)                                  PACKER 101
 8001 FORMAT(1H ,15X,3A4,12H CHANGED BY ,F5.1,9H AT HOUR ,I2)                   PACKER 102
 8002 FORMAT(1H ,15X,3A4,13H MISSING FOR ,I2,6H HOURS)                          PACKER 103
 8003 FORMAT(////1H1,14H--- ERROR --- ,22HONE OR MORE VARIABLES ,               PACKER 104
     1 59HHAVE NO DATA FOR THE FIRST DAY.  DUMP OF DAYS DATA FOLLOWS.//)        PACKER 105
 8004 FORMAT(1H ,4X,3A4,4X,3A4,4X,3A4,8X,3A4,2X,3A4,2X,3A4,2X,3A4,2X,3A4        PACKER 106
     1         //(1H ,8I16))                                                    PACKER 107
 8005 FORMAT(////1H ,14H--- ERROR --- ,19HEND OF FILE READ.  ,                  PACKER 108
     1       10HTAPE TYPE ,A4,A2)                                               PACKER 109
 8006 FORMAT(1H ,15X,26HINTERPOLATION INTERVAL OF ,I2,                          PACKER 110
     1       20H HOURS EXCEEDED FOR ,3A4)                                       PACKER 111
 8007 FORMAT(15H --- ERROR --- ,18HINVALID MONTH NAME )                         PACKER 112
 8008 FORMAT(' --- ERROR --- INCORRECT STATION NUMBER')                         -089    10
C                                                                               PACKER 113
      FIRSTD = .TRUE.                                                           PACKER 114
      OUTWTH = 1                                                                PACKER 115
      CALCGT = .FALSE.                                                          PACKER 116
      DBTYAV = 0.0                                                              PACKER 117
      NCALL = 0                                                                 PACKER 118
      DO 2 IM=1,12                                                              PACKER 119
    2 DBTAV(IM) = 0.0                                                           PACKER 120
C              READ IN STATION NAME                                             PACKER 121
      READ(INFIL,5000,END=3100)  IWDID                                          PACKER 122
      IF( IWDID(1) .EQ. IEND )  GOTO  3100                                      PACKER 123
C              ECHO OUT STATION NAME                                            PACKER 124
      WRITE(OUTFIL,6000) IWDID                                                  PACKER 125
C              READ IN PACKER DATA                                              PACKER 126
      READ(INFIL,5001,END=3100)  ITAPE,ISTAT,IYR,NTZ,XLAT,XLONG,                PACKER 127
     1                           IBITS,IFILE,INTINT,DDBT,DIFF,ALT,ILOC          073a     8
      IF( ITAPE(1) .EQ. IEND )  GOTO  3100                                      PACKER 129
C              VERIFY INPUT VARIABLES                                           PACKER 130
      CALL VALIDA(ITAPE(1),NAMETT(1,1),17,NAMS(1,1),STOPIT,ITPTYP)              -089    11
C                                                                               PACKER 132
C              ITPTYP CODE                                                      PACKER 133
C                    1 = ZZZZZZ                                                 PACKER 134
C                    2 = OTHER                                                  PACKER 135
C                    3 = TRY                                                    PACKER 136
C                    4 = 1440                                                   PACKER 137
C                    5 = 1440-3                                                 PACKER 138
C                    6 = CTZ                                                    PACKER 139
C                    7 = TMY                                                    PACKER 140
C                    8 = SOLMET                                                 PACKER 141
C                    9 = WYEC                                                   PACKER 142
C                   10 = TRY (KIMURA-STEPHENSON)                                PACKER 143
C                   11 = CD144                                                  PACKER 144
C                   12 = CD144 SOLAR                                            CD144S  61
C                   13 = TD9685                                                 PACKER 146
C                   14 = CANADA                                                 WTH2    19
C                   15 = WYEC2                                                  WTH2    20
C                   16 = TRY SOLAR                                              CD144S  62
C                   17 = TMY2                                                   CD144S  63
C              NOTICE THAT 1 IS SUBTRACTED FROM ITPTYP BELOW                    PACKER 147
C                                                                               PACKER 148
      CALL VALIDI(ISTAT,-1,99999,NAMS(1,2),0,STOPIT)                            PACKER 149
      IF (ITPTYP.EQ.17) THEN                                                    -089    12
          READ(INWTH,5004,END=3600) KSTAT,JCITY,JSTATE,JTZ,JNS,JLATDG,          -089    13
     1                              JLATMN,JEW,JLONGD,JLONGM,JELEV              -089    14
          IF (KSTAT.NE.ISTAT) THEN                                              -089    15
              WRITE(OUTFIL,8008)                                                -089    16
              STOPIT = .TRUE.                                                   -089    17
              RETURN                                                            -089    18
          END IF                                                                -089    19
          ISTAT = KSTAT                                                         -089    20
          NTZ = -JTZ                                                            -089    21
          XLAT = FLOAT(JLATDG) + FLOAT(JLATMN)/60.                              -089    22
          XLONG = FLOAT(JLONGD) + FLOAT(JLONGM)/60.                             -089    23
          IF (JNS.EQ.'S') XLAT = -XLAT                                          -089    24
          IF (JEW.EQ.'E') XLONG = -XLONG                                        -089    25
          ALT = FLOAT(JELEV)                                                    -089    26
          IWSZ = 2                                                              -089    27
          IFTYP = 3                                                             -089    28
          INTINT = 1                                                            -089    29
          DDBT = 20.                                                            -089    30
          DIFF = .025                                                           -089    31
          ILOC = 1                                                              -089    32
          DO I=1,12                                                             -089    33
              CN(I) = 1.0                                                       -089    34
              TG(I) = 60.                                                       -089    35
          END DO                                                                -089    36
          CALCGT = .TRUE.                                                       -089    37
          OUTWTH = 11                                                           -089    38
          OPEN (11, FILE='SCR.TMP',FORM='UNFORMATTED',STATUS='UNKNOWN')         -089    39
          STALAT = DTOR * XLAT                                                  -089    40
          STALON = DTOR * XLONG                                                 -089    41
          SSTALA = SIN(STALAT)                                                  -089    42
          CSTALA = COS(STALAT)                                                  -089    43
          TSTALA = SSTALA/CSTALA                                                -089    44
          HRSLON = XLONG/15.                                                    -089    45
          ITPTYP = ITPTYP - 1                                                   -089    46
          GO TO 970                                                             -089    47
 3600     WRITE(OUTFIL,8005) NAMETT(1,ITPTYP), NAMETT(2,ITPTYP)                 -089    48
          STOPIT = .TRUE.                                                       -089    49
          RETURN                                                                -089    50
      END IF                                                                    -089    51
      CALL VALIDI(IYR,-1,1999,NAMS(1,3),0,STOPIT)                               PACKER 150
      CALL VALIDI(NTZ,-12,12,NAMS(1,4),0,STOPIT)                                PACKER 151
      CALL VALIDF(XLAT,-90.,90.,NAMS(1,5),0,STOPIT)                             PACKER 152
      CALL VALIDF(XLONG,-180.,180.,NAMS(1,6),0,STOPIT)                          PACKER 153
      CALL VALIDA(IBITS(1),IWDSZ(1,1),2,NAMS(1,7),STOPIT,IWSZ)                  PACKER 154
      CALL VALIDA(IFILE(1),IFLNAM(1,1),3,NAMS(1,13),STOPIT,IFTYP)               PACKER 155
      IF ((IFTYP.NE.3) .AND. (ITPTYP.EQ.7 .OR. ITPTYP.EQ.8 .OR.                 CD144S  64
     1  ITPTYP.EQ.9 .OR. ITPTYP.EQ.12 .OR. ITPTYP.EQ.15 .OR.                    CD144S  65
     2  ITPTYP.EQ.16)) THEN                                                     CD144S  66
        IFTYP = 3                                                               CD144S  67
        WRITE(OUTFIL,9002)                                                      CD144S  68
      END IF                                                                    CD144S  69
 9002 FORMAT(1H ,'--- WARNING --- "SOLAR" WILL BE USED WITH CD144S, ',          CD144S  70
     1 'WYEC2, TRYSLM, AND TMY2 SOURCE DATA')                                   CD144S  71
      CALL VALIDI(INTINT,1,24,NAMS(1,9),0,STOPIT)                               PACKER 160
      CALL VALIDF(DDBT,10.,100.,NAMS(1,10),0,STOPIT)                            PACKER 161
      IF (DIFF .NE. 0.0)  CALL VALIDF(DIFF,0.,1.0,NAMS(1,14),0,STOPIT)          PACKER 162
      IF (ALT .NE. 0)  CALL VALIDF(ALT,-1000.,15000.,NAMS(1,15),                PACKER 163
     1                             0,STOPIT)                                    PACKER 164
      IF (ILOC.NE.0) CALL VALIDI(ILOC,1,26,NAMS(1,17),0,STOPIT)                 073a    10
      IF (ILOC.EQ.0) ILOC=1                                                     073a    11
      STALAT = DTOR * XLAT                                                      LOCALC   2
      STALON = DTOR * XLONG                                                     LOCALC   3
      SSTALA = SIN(STALAT)                                                      LOCALC   4
      CSTALA = COS(STALAT)                                                      LOCALC   5
      TSTALA = SSTALA/CSTALA                                                    LOCALC   6
      HRSLON = XLONG/15.                                                        LOCALC   7
C              READ CLEARNESS NUMBERS AND VALIDATE                              PACKER 166
      READ(INFIL,5002,END=3100)  CN                                             PACKER 167
      DO 3 I = 1,12                                                             PACKER 168
      NCNT   = I                                                                PACKER 169
      CALL VALIDF(CN(NCNT),0.0,1.2,NAMS(1,11),NCNT,STOPIT)                      PACKER 170
    3 CONTINUE                                                                  PACKER 171
C              READ IN GROUND TEMPERATURES AND VALIDATE                         PACKER 172
      READ(INFIL,5002,END=3100)  TG                                             PACKER 173
      IF (TG(1) .EQ. UNDEF)  CALCGT = .TRUE.                                    PACKER 174
      IF (CALCGT) THEN                                                          WTHWF   13
          OUTWTH = 11                                                           WTHWF   14
          OPEN ( 11, FILE='SCR.TMP', FORM='UNFORMATTED'                         WTHWF   15
     $                             , STATUS='UNKNOWN' ,                         WTHWF   16
     $      recordtype='variable',recl=6200,blocksize=148992)                   WTHWF   17
          GO TO 88                                                              WTHWF   18
      ENDIF                                                                     WTHWF   19
      DO 8 I = 1,12                                                             PACKER 179
      NCNT   = I                                                                PACKER 180
      CALL VALIDF(TG(NCNT),-100.,200.,NAMS(1,12),NCNT,STOPIT)                   PACKER 181
    8 CONTINUE                                                                  PACKER 182
   88 IF (STOPIT)  GO TO 3200                                                   PACKER 183
      ITPTYP = ITPTYP - 1                                                       PACKER 184
C              READ IN DATES AND DIRECTION OF SOLAR BINSHIFTS                   PACKER 185
C              FOR WYEC TAPES                                                   PACKER 186
      IF (ITPTYP .NE. 8)  GO TO 970                                             PACKER 187
      ISHFT  = 1                                                                PACKER 188
      READ (INFIL,5003,END=3100) NBS,(MSHFTN(I),IDSHFT(I),I=1,5),               PACKER 189
     1                           (ISSHFT(I),I=1,5)                              PACKER 190
      WRITE (OUTFIL,6005)                                                       PACKER 191
      DO 960 I=1,NBS                                                            PACKER 192
      MSHFT  = 0                                                                PACKER 193
      DO 940 J=1,12                                                             PACKER 194
      IF (MSHFTN(I) .NE. MNAMES(J))  GO TO 940                                  PACKER 195
      MSHFT  = J                                                                PACKER 196
      GO TO 950                                                                 PACKER 197
  940 CONTINUE                                                                  PACKER 198
      WRITE (OUTFIL,8007)                                                       PACKER 199
      GO TO 3100                                                                PACKER 200
  950 IDOYSH(I) = BEFORE(MSHFT) + IDSHFT(I)                                     PACKER 201
      WRITE (OUTFIL,6006) MSHFTN(I),IDSHFT(I),ISSHFT(I)                         PACKER 202
C              PRINT 9001,IDOYSH(I)                                             PACKER 203
 9001 FORMAT(1H ,9HIDOYSH = ,I11)                                               PACKER 204
  960 CONTINUE                                                                  PACKER 205
  970 CONTINUE                                                                  PACKER 206
      IF (ITPTYP .EQ. 13) THEN                                                  WTH2    25
          READ(INFIL,5002,END=3100) (SKYCOV(I),I=1,12)                          WTH2    26
          DO 980 I=1,12                                                         WTH2    27
              CALL VALIDF(SKYCOV(I),0.,10.,NAMS(1,16),I,STOPIT)                 WTH2    28
  980     CONTINUE                                                              WTH2    29
      ELSE                                                                      WTH2    30
          DO 990 I=1,12                                                         WTH2    31
              SKYCOV(I) = 0.                                                    WTH2    32
  990     CONTINUE                                                              WTH2    33
      END IF                                                                    WTH2    34
C              CHECK FOR 3RD HOUR TAPES AND SET BEGINNING HOUR                  PACKER 207
      IBEGH  = 1                                                                PACKER 208
      IF ((ITPTYP .EQ. 4) .OR. (ITPTYP .EQ. 11))                                PACKER 209
     1   IBEGH = ((IABS(NTZ)+2)/3)*3 - IABS(NTZ) + 1                            073b    64
C              ZERO PREVIOUS HOUR ARRAY                                         PACKER 211
      DO I=1,12                                                                 CD144S  72
        TG(I) = TG(I) + 460.                                                    CD144S  73
      END DO                                                                    CD144S  74
      DO I=1,14                                                                 CD144S  75
        IRAWP(I) = 0                                                            CD144S  76
      END DO                                                                    CD144S  77
      IRAWP(10) = 2                                                             PACKER 215
C              LOOP OVER YEARS FOR SOLMET DATA                                  PACKER 216
      NYR    = 1                                                                PACKER 217
      DO 2000 IYRC=1,NYR                                                        PACKER 218
C              LOOP OVER MONTHS                                                 PACKER 219
C              DO PAGE EJECT                                                    PACKER 220
      WRITE(OUTFIL,6004)                                                        PACKER 221
      FIRSTS = .TRUE.                                                           PACKER 222
      DO 2000 IM=1,12                                                           PACKER 223
      IMNTH  = IM                                                               PACKER 224
C                                                                               PACKER 225
C              PUT FIRST DAYS DATA IN IRAW3                                     PACKER 226
      IF (.NOT. FIRSTD)  GO TO 20                                               PACKER 227
      IDAY   = 1                                                                PACKER 228
      DO I=1,14                                                                 CD144S  78
        DO J=1,24                                                               CD144S  79
          IRAW(J,I) = IUNDEF                                                    CD144S  80
        END DO                                                                  CD144S  81
      END DO                                                                    CD144S  82
      IF (ITPTYP .EQ. 1)  CALL OTHER                                            PACKER 229
      IF (ITPTYP .EQ. 2)  CALL TRYDCD                                           PACKER 230
      IF (ITPTYP .EQ. 3 .OR. ITPTYP .EQ. 4)  CALL WXDCD                         PACKER 231
      IF (ITPTYP .EQ. 5)  CALL CTZDCD                                           PACKER 232
      IF (ITPTYP .EQ. 6)  CALL TMYDCD                                           PACKER 233
      IF (ITPTYP .EQ. 7)  CALL TMYDCD                                           PACKER 234
      IF (ITPTYP .EQ. 8)  CALL TRYDCD                                           PACKER 235
      IF (ITPTYP .EQ. 9)  CALL TRYDCD                                           PACKER 236
      IF (ITPTYP .EQ. 10)  CALL CTZDCD                                          PACKER 237
      IF (ITPTYP .EQ. 11)  CALL CTZDCD                                          PACKER 238
      IF (ITPTYP .EQ. 12) CALL DC9685                                           PACKER 239
      IF (ITPTYP .EQ. 13)  CALL CANDCD                                          WTH2    35
      IF (ITPTYP .EQ. 14) CALL WY2DCD                                           WTH2    36
      IF (ITPTYP.EQ.15) CALL TRYDCD                                             073a    12
      IF (ITPTYP.EQ.16) CALL TMY2DCD                                            -089    52
      FIRSTD = .FALSE.                                                          PACKER 240
      FIRSTS = .FALSE.                                                          PACKER 241
      IF (IEOF) GO TO 3400                                                      PACKER 242
      DO 10 I=1,14                                                              CD144S  83
      DO 10 J=1,24                                                              PACKER 244
      IRAW3(J+24,I) = IRAW(J,I)                                                 PACKER 245
      IRAW3(J+48,I) = IRAW(J,I)                                                 PACKER 246
   10 CONTINUE                                                                  PACKER 247
C              Decide whether to use humidity ratio or wetbulb/dewpoint data    CD144S  84
      NWB = 0                                                                   CD144S  85
      NDP = 0                                                                   CD144S  86
      NRH = 0                                                                   CD144S  87
      NDB = 0                                                                   CD144S  88
      DO I=1,24                                                                 CD144S  89
        IF (IDRY(I).NE.IUNDEF) NDB = NDB + 1                                    CD144S  90
        IF (IWET(I).NE.IUNDEF) NWB = NWB + 1                                    CD144S  91
        IF (IDEW(I).NE.IUNDEF) NDP = NDP + 1                                    CD144S  92
        IF (IRELH(I).NE.IUNDEF) NRH = NRH + 1                                   CD144S  93
      END DO                                                                    CD144S  94
      IF ((ITPTYP.EQ.10 .OR.ITPTYP.EQ.11) .AND. NDB.GE.4 .AND. NRH.GE.4         CD144S  95
     1    .AND. (NWB.LE.3 .OR. NDP.LE.3)) THEN                                  CD144S  96
          RHCALC = .TRUE.                                                       CD144S  97
      ELSE                                                                      CD144S  98
          RHCALC = .FALSE.                                                      CD144S  99
      END IF                                                                    CD144S 100
C              INITIALIZE PREVIOUS HOURS ARRAYS                                 PACKER 248
      DO 18 IV=1,14                                                             CD144S 101
      NHRP(IV) = 0                                                              PACKER 250
      J      = 49                                                               PACKER 251
   12 IF (IRAW3(J,IV) .NE. IUNDEF)  GO TO 15                                    PACKER 252
      IF (IFTYP .LT. 3 .AND. (IV.EQ.7 .OR. IV.EQ.8))  GO TO 15                  PACKER 253
      IF ((ITPTYP.EQ.11 .OR. ITPTYP.EQ.15) .AND. (IV.EQ.7 .OR. IV.EQ.8))        CD144S 102
     1    GO TO 18                                                              CD144S 103
      J      = J - 1                                                            PACKER 254
      NHRP(IV) = NHRP(IV) + 1                                                   PACKER 255
      IF (NHRP(IV) .GT. 24) THEN                                                WTH1     4
          IF (IV .EQ. 4) THEN                                                   WTH1     5
              IRAWP(4) = IROUND(2992.*EXP(-.0000368*ALT))                       073b    65
              GO TO 18                                                          WTH1     7
          END IF                                                                WTH1     8
          IF (IV .EQ. 6) THEN                                                   WTH1     9
              IRAWP(6) = 5                                                      WTH1    10
              GO TO 18                                                          WTH1    11
          END IF                                                                WTH1    12
          IF (IV .EQ. 13) THEN                                                  CD144S 104
              IRAWP(13) = 5                                                     CD144S 105
              GO TO 18                                                          CD144S 106
          END IF                                                                CD144S 107
          IF ((IV.EQ.2 .OR. IV.EQ.3) .AND. RHCALC) GO TO 18                     CD144S 108
          IF (IV.EQ.14 .AND. .NOT.RHCALC) GO TO 18                              CD144S 109
          IF (IV .LE. 8) GO TO 3500                                             WTH1    13
          GO TO 18                                                              WTH1    14
      END IF                                                                    WTH1    15
      GO TO 12                                                                  PACKER 259
   15 IRAWP(IV) = IRAW3(J,IV)                                                   PACKER 260
   18 CONTINUE                                                                  PACKER 261
   20 CONTINUE                                                                  PACKER 264
C              LOOP THROUGH DAYS IN MONTH                                       PACKER 265
      NOD    = MDAYS(IM)                                                        073b    66
      DO 1000 ID=1,NOD                                                          PACKER 267
      IDAY   = ID + 1                                                           PACKER 268
      IF (IDAY.GT.MDAYS(IM)) THEN                                               073b    67
          IDAY = 1                                                              073b    68
          IMNTH = IMNTH + 1                                                     073b    69
      END IF                                                                    073b    70
      IDOY = BEFORE(IM) + ID                                                    CD144S 110
C              SET DAY AND RECORD INDICES FOR OUTPUT                            PACKER 269
      IRECXO = IM                                                               073b    71
      IDXO   = ID                                                               PACKER 271
      IF (IFTYP .EQ. 1)  GO TO 25                                               PACKER 272
      IRECXO = IM*2 + (ID-1)/16 - 1                                             073b    72
      IDXO   = MOD(ID-1,16) + 1                                                 PACKER 274
   25 CONTINUE                                                                  PACKER 275
      FIRSTE = .TRUE.                                                           PACKER 276
C SET UP LST ARRAY FOR CURRENT DAY; HARMLESS IF NOT TMY OR SOLMET               PACKER 277
      DO 70 I=1,24                                                              PACKER 278
         LSTHRX(I) = LSTHRS(I)                                                  PACKER 279
70    CONTINUE                                                                  PACKER 280
      KYRL = KYR                                                                -089    53
C              IF LAST DAY OF YEAR, DONT GET DATA                               PACKER 281
      IF(ITPTYP.NE.7) GO TO 26                                                  PACKER 282
      IF(IYRC.LT.IYR) GO TO 29                                                  PACKER 283
   26 CONTINUE                                                                  PACKER 284
      IF ((IM .EQ. 12) .AND. (ID .EQ. 31)) GO TO 30                             073b    73
      DO I=1,14                                                                 CD144S 111
        DO J=1,24                                                               CD144S 112
          IRAW(J,I) = iundef                                                    CD144S 113
        END DO                                                                  CD144S 114
      END DO                                                                    CD144S 115
      IF (ITPTYP .EQ. 1)  CALL OTHER                                            PACKER 287
      IF (ITPTYP .EQ. 2)  CALL TRYDCD                                           PACKER 288
      IF (ITPTYP .EQ. 3 .OR. ITPTYP .EQ. 4)  CALL WXDCD                         PACKER 289
      IF (ITPTYP .EQ. 5)  CALL CTZDCD                                           PACKER 290
      IF (ITPTYP .EQ. 6)  CALL TMYDCD                                           PACKER 291
   29 IF (ITPTYP .EQ. 7)  CALL TMYDCD                                           PACKER 292
      IF (ITPTYP .EQ. 8)  CALL TRYDCD                                           PACKER 293
      IF (ITPTYP .EQ. 9)  CALL TRYDCD                                           PACKER 294
      IF (ITPTYP .EQ. 10)  CALL CTZDCD                                          PACKER 295
      IF (ITPTYP .EQ. 11)  CALL CTZDCD                                          PACKER 296
      IF (ITPTYP .EQ. 12) CALL DC9685                                           PACKER 297
      IF (ITPTYP .EQ. 13)  CALL CANDCD                                          WTH2    37
      IF (ITPTYP .EQ. 14) CALL WY2DCD                                           WTH2    38
      IF (ITPTYP.EQ.15) CALL TRYDCD                                             073a    13
      IF (ITPTYP.EQ.16) CALL TMY2DCD                                            -089    54
      IF (IEOF)  GO TO 3400                                                     PACKER 298
C              MOVE DATA DOWN IN IRAW3.  ZERO ERROR COUNTS.                     PACKER 299
   30 CONTINUE                                                                  PACKER 300
      DO 60 I = 1,14                                                            CD144S 116
      NHRE(I) = 0                                                               PACKER 302
      NHRE2(I) = 0                                                              PACKER 303
      DO 40 J=1,48                                                              PACKER 304
      IRAW3(J,I) = IRAW3(J+24,I)                                                PACKER 305
   40 CONTINUE                                                                  PACKER 306
      DO 50 J=1,24                                                              PACKER 307
      IRAW3(J+48,I) = IRAW(J,I)                                                 PACKER 308
   50 CONTINUE                                                                  PACKER 309
   60 CONTINUE                                                                  PACKER 310
C              LOOP OVER HOURS IN PRESENT DAY                                   PACKER 311
      DO 200 IH=1,24                                                            PACKER 312
      IHOUR  = IH                                                               PACKER 313
      IHNOW  = IH + 24                                                          PACKER 314
      IHNOW1  = IH + 24                                                         PACKER 315
      IHNOW2  = IHNOW1                                                          PACKER 316
      IF (ITPTYP .NE. 6 .AND. ITPTYP .NE. 7) GOTO 105                           PACKER 317
         IHDIF = IH - LSTHRX(IH)                                                PACKER 318
         IF (IHDIF .GT.  12) IHDIF = IHDIF - 24                                 PACKER 319
         IF (IHDIF .LT. -12) IHDIF = IHDIF + 24                                 PACKER 320
         IHNOW2 = IHNOW2 + IHDIF                                                PACKER 321
105   CONTINUE                                                                  PACKER 322
C              FILL FLOATING POINT HOURLY WEATHER ARRAY, WD()                   PACKER 323
C                    WD(1) = TDB                                                PACKER 324
C                    WD(2) = TWB                                                PACKER 325
C                    WD(3) = TDP                                                PACKER 326
C                    WD(4) = BAROM                                              PACKER 327
C                    WD(5) = WNDSP                                              PACKER 328
C                    WD(6) = CLDAMT                                             PACKER 329
C                    WD(7) = TOT HRIZ SOLAR                                     PACKER 330
C                    WD(8) = DIR NRML SOLAR                                     PACKER 331
C                    WD(9) = OPAQUE CLOUD AMOUNT                                CD144S 117
C                    WD(10) = RELATIVE HUMIDITY                                 CD144S 118
      DO 150 IFV = 1,10                                                         CD144S 119
      IV = IFV                                                                  CD144S 120
      IF (IFV.EQ.9) IV=13                                                       CD144S 121
      IF (IFV.EQ.10) IV=14                                                      CD144S 122
      IHNOW = IHNOW2                                                            PACKER 333
      IF (IFV.EQ.7  .OR. IFV.EQ.8) IHNOW = IHNOW1                               CD144S 123
      IPRESV = IRAW3(IHNOW,IV)                                                  PACKER 335
      IF (IFTYP.LT.3 .AND.(IFV.EQ.7 .OR. IFV.EQ.8)) GO TO 115                   CD144S 124
      IF (ITPTYP.EQ.11 .AND.(IFV.EQ.7 .OR. IFV.EQ.8)) GO TO 115                 CD144S 125
      IF (ITPTYP.EQ.15 .AND.(IFV.EQ.7 .OR. IFV.EQ.8)) GO TO 115                 CD144S 126
      IF (IFV.EQ.10 .AND. .NOT.RHCALC) GO TO 115                                CD144S 127
      IF (IFV.EQ.2 .AND. RHCALC) GO TO 115                                      CD144S 128
      IF (IFV.EQ.3 .AND. RHCALC) GO TO 115                                      CD144S 129
C              IS THERE A VALUE THIS HOUR                                       PACKER 338
      IF (IPRESV .EQ. IUNDEF)  GO TO 120                                        PACKER 339
C              THERE IS A VALUE.  CHECK FOR DBT CHANGE                          PACKER 340
      IF (IFV.GT.1)  GO TO 110                                                  CD144S 130
      DELT   = FLOAT(IPRESV-IRAWP(IV))                                          PACKER 342
      IF (ABS(DELT) .LT. DDBT)  GO TO 110                                       PACKER 343
      IF (FIRSTE)  WRITE(OUTFIL,8000) MNAMES(IM), ID                            073b    74
      FIRSTE = .FALSE.                                                          PACKER 345
      WRITE(OUTFIL,8001) (NAMES(I,IV),I=1,3), DELT, IH                          PACKER 346
  110 CONTINUE                                                                  PACKER 347
C              FILL WD, RESET PREVIOUS VALUE                                    PACKER 348
      WD(IFV) = FLOAT(IPRESV)                                                   CD144S 131
  114 IRAWP(IV) = IPRESV                                                        PACKER 350
      NHRP(IV) = 0                                                              PACKER 351
      GO TO 150                                                                 PACKER 352
  115 WD(IFV) = 0.                                                              CD144S 132
      GO TO 114                                                                 PACKER 354
  120 CONTINUE                                                                  PACKER 355
C              VALUE MISSING. INCREMENT MISSING HOUR COUNT                      PACKER 356
      NHRE(IV) = NHRE(IV) + 1                                                   PACKER 357
C              INCREMENT PREVIOUS HOUR COUNT                                    PACKER 358
      NHRP(IV) = NHRP(IV) + 1                                                   PACKER 359
      IF (NHRP(IV) .GT. 24)  GO TO 140                                          PACKER 360
C              GET NEXT OCCURENCE OF DATA                                       PACKER 361
      NHRN   = 0                                                                PACKER 362
  130 NHRN   = NHRN + 1                                                         PACKER 363
      IHRX = NHRN + IHNOW                                                       PACKER 364
      IF (IHRX .GT. 72) GO TO 140                                               PACKER 365
      IF (NHRN .GT. 24)  GO TO 140                                              PACKER 366
      IHRX   = NHRN + IH + 24                                                   PACKER 367
      IF (IRAW3(IHRX,IV) .EQ. IUNDEF)  GO TO 130                                PACKER 368
      IF ((NHRN+NHRP(IV)) .GT. 24)  GO TO 140                                   PACKER 369
      IF ((NHRN+NHRP(IV)) .GT. INTINT)  NHRE2(IV) = 1                           PACKER 370
C              CALCULATE INTERPOLATED VALUE                                     PACKER 371
      WD(IFV) = FLOAT(IRAWP(IV)) + FLOAT(IRAW3(IHRX,IV)-IRAWP(IV))              CD144S 133
     1               *FLOAT(NHRP(IV))/FLOAT(NHRP(IV)+NHRN)                      PACKER 373
      GO TO 150                                                                 PACKER 374
  140 NHRE2(IV) = 1                                                             PACKER 375
      WD(IFV) = FLOAT(IRAWP(IV))                                                CD144S 134
  150 CONTINUE                                                                  PACKER 377
C              STORE INTEGER VALUES IN IWD()                                    PACKER 378
      IHNOW = IHNOW1                                                            PACKER 379
      DO 170 IIV=1,4                                                            CD144S 135
      IV   = IIV + 8                                                            CD144S 136
      IPRESV = IRAW3(IHNOW,IV)                                                  CD144S 137
      IF (IPRESV .EQ. IUNDEF)  GO TO 160                                        CD144S 138
      IWD(IIV) = IPRESV                                                         CD144S 139
      IRAWP(IV) = IPRESV                                                        CD144S 140
      GO TO 170                                                                 CD144S 141
  160 IWD(IIV) = IRAWP(IV)                                                      CD144S 142
      NHRE(IV) = NHRE(IV) + 1                                                   CD144S 143
  170 CONTINUE                                                                  PACKER 390
C              FILL WEATHD BY ASSIGNMENT                                        PACKER 391
      DBT    = WD(1)                                                            PACKER 392
      DBTAV(IM) = DBTAV(IM) + DBT                                               073b    75
      PATM   = WD(4)*0.01                                                       PACKER 396
      WNDSPD = WD(5)                                                            PACKER 397
      IF (IFTYP .EQ. 1)  WNDSPD = 1.1508*WNDSPD                                 PACKER 398
      CLDAMT = WD(6)                                                            PACKER 399
      SOLRAD = WD(7)                                                            PACKER 400
      DIRSOL = WD(8)                                                            PACKER 401
      COPQ = WD(9)                                                              CD144S 144
      IWNDDR = IWD(1)                                                           PACKER 402
      ICLDTY = IWD(2)                                                           PACKER 403
      IRAIN  = IWD(3)                                                           PACKER 404
      ISNOW  = IWD(4)                                                           PACKER 405
      IF (ITPTYP.EQ.11 .OR. ITPTYP.EQ.15) THEN                                  CD144S 145
        IF (IH.EQ.1) CALL SUNPRM(IDOY,DEABC)                                    CD144S 146
        CALL SUNCOS(IH,DEABC,COSZIN,COSZAV,SALT,FSUN)                           CD144S 147
        CALL SUNCLR(IM,DEABC,COSZAV,DNCLR,DFCLR,TOTCLR)                         CD144S 148
        CCF = CCSLMT(COPQ/10.,IRAIN,ILOC)                                       CD144S 149
        TOTHOR = FSUN*CCF*TOTCLR                                                CD144S 150
        CALL SUNSPL(IDOY,COSZIN,COSZAV,TOTHOR,DIRH,DIRNM,DIF)                   CD144S 151
        SOLRAD = DIRH + DIF                                                     CD144S 152
        DIRSOL = DIRNM                                                          CD144S 153
      END IF                                                                    CD144S 154
      IF (RHCALC) THEN                                                          CD144S 155
        RH = .01*WD(10)                                                         CD144S 156
        PS = PPWVMS(DBT)                                                        CD144S 157
        PPWV = RH*PS                                                            CD144S 158
        HUMRAT = 0.622*PPWV/(PATM-PPWV)                                         CD144S 159
        ENTHAL = 0.24*DBT+(1061.0+0.444*DBT)*HUMRAT                             CD144S 160
        WBT = WBF(ENTHAL,PATM)                                                  CD144S 161
        WBT = AMIN1(WBT,DBT)                                                    CD144S 162
      ELSE                                                                      CD144S 163
        WBT = AMIN1(WD(2),DBT)                                                  CD144S 164
        DPT = AMIN1(WD(3),WBT)                                                  CD144S 165
        IF (DPT.GT.32.) THEN                                                    CD144S 166
          PPWV = PPWVMS(WBT)-0.000367*PATM*(DBT-WBT)/                           CD144S 167
     1           (1.0+(WBT-32.0)/1571.0)                                        CD144S 168
        ELSE                                                                    CD144S 169
          PPWV = PPWVMS(DPT)                                                    CD144S 170
        END IF                                                                  CD144S 171
        HUMRAT = 0.622*PPWV/(PATM-PPWV)                                         CD144S 172
        ENTHAL = 0.24*DBT+(1061.0+0.444*DBT)*HUMRAT                             CD144S 173
      END IF                                                                    CD144S 174
      DENSTY = 1.0/(0.754*(DBT+459.7)*(1.0+1.606*HUMRAT)/PATM)                  CD144S 175
      CALL WPACK                                                                PACKER 417
      IWSOL  = IWSZ + (IFTYP-1)*2 - 1                                           PACKER 418
  200 CONTINUE                                                                  PACKER 419
C              LOOP THROUGH ERRORS                                              PACKER 420
      DO 300 I = 1,10                                                           PACKER 421
      IF (IFTYP.NE.3 .AND. (I.EQ.7 .OR. I.EQ.8) ) GO TO 300                     PACKER 422
C              CHECK FOR 3RD HOUR TAPES                                         PACKER 423
      IF (ITPTYP .EQ. 4)  NHRE(I) = NHRE(I) - 16                                PACKER 424
      IF (NHRE(I) .LE. 0 .AND. NHRE2(I) .EQ. 0)  GO TO 300                      PACKER 425
      IF (FIRSTE)  WRITE(OUTFIL,8000) MNAMES(IM), ID                            073b    76
      FIRSTE = .FALSE.                                                          PACKER 427
      IF (NHRE(I) .GT. 0)  WRITE(OUTFIL,8002) (NAMES(J,I),J=1,3),               PACKER 428
     1                                        NHRE(I)                           PACKER 429
      IF (NHRE2(I) .GT. 0)  WRITE(OUTFIL,8006) INTINT,                          PACKER 430
     1 (NAMES(J,I),J=1,3)                                                       PACKER 431
  300 CONTINUE                                                                  PACKER 432
      IF (ID .NE. 16 .AND. ID .NE. NOD)  GO TO 1000                             PACKER 433
      IWSP1  = IWSOL + 1                                                        PACKER 434
      IF (KYRL.GE.50 .AND. KYRL.LE.99) THEN                                     -089    55
          KYRL = KYRL + 1900                                                    -089    56
      ELSE IF (KYRL.GE.0 .AND. KYRL.LE.49) THEN                                 -089    57
          KYRL = KYRL + 2000                                                    -089    58
      END IF                                                                    -089    59
      GO TO (400,400,500,600,500,600),IWSP1                                     PACKER 435
  400 IF (ID .NE. NOD)  GO TO 1000                                              PACKER 436
      WRITE (OUTWTH)  IWDID(1),IWDID(2),KYRL,XLAT,XLONG,NTZ,IM,NOD,             -089    60
     1                CN(IM),TG(IM),IWSOL,IDATO                                 073b    78
      GO TO 1000                                                                PACKER 439
  500 WRITE (OUTWTH)  IWDID,KYRL,XLAT,XLONG,NTZ,IRECXO,NOD,CN(IM),              -089    61
     1                TG(IM),IWSOL,IDAT60                                       073b    80
      GO TO 1000                                                                PACKER 442
  600 WRITE (OUTWTH)  IWDID,KYRL,XLAT,XLONG,NTZ,IRECXO,NOD,CN(IM),              -089    62
     1                TG(IM),IWSOL,IDAT30                                       073b    82
 1000 CONTINUE                                                                  PACKER 445
 2000 CONTINUE                                                                  PACKER 446
      IF (.NOT.CALCGT)  RETURN                                                  PACKER 447
      DO 2100 IM=1,12                                                           PACKER 448
      DBTYAV = DBTYAV + DBTAV(IM)                                               PACKER 449
 2100 DBTAV(IM) = DBTAV(IM)/(24.*FLOAT(MDAYS(IM)))                              PACKER 450
      DBTYAV = DBTYAV/8760.                                                     PACKER 451
      DBTMAX = AMAX1(DBTAV(1),DBTAV(2),DBTAV(3),DBTAV(4),DBTAV(5),              PACKER 452
     1 DBTAV(6),DBTAV(7),DBTAV(8),DBTAV(9),DBTAV(10),DBTAV(11),                 PACKER 453
     2 DBTAV(12))                                                               PACKER 454
      DBTMIN = AMIN1(DBTAV(1),DBTAV(2),DBTAV(3),DBTAV(4),DBTAV(5),              PACKER 455
     1 DBTAV(6),DBTAV(7),DBTAV(8),DBTAV(9),DBTAV(10),DBTAV(11),                 PACKER 456
     2 DBTAV(12))                                                               PACKER 457
      CALL GTEMP(DIFF,DBTMIN,DBTMAX,DBTYAV,TG)                                  PACKER 458
      REWIND OUTWTH                                                             PACKER 459
      OUTWTH = 1                                                                PACKER 460
      GO TO (2400,2400,2500,2600,2500,2600), IWSP1                              PACKER 461
 2400 DO 2450 IM=1,12                                                           PACKER 462
      READ (SOLWTH) IWDID(1),IWDID(2),KYR,XLAT,XLONG,NTZ,IMNTH,NOD,             PACKER 463
     1          CLRNES,DUM,IWSOL,IDATO                                          PACKER 464
      WRITE (OUTWTH) IWDID(1),IWDID(2),KYR,XLAT,XLONG,NTZ,IMNTH,NOD,            PACKER 465
     1          CLRNES,TG(IM),IWSOL,IDATO                                       PACKER 466
 2450 CONTINUE                                                                  PACKER 467
      RETURN                                                                    PACKER 468
 2500 DO 2550 IM=1,24                                                           PACKER 469
      IM2    = (IM-1)/2 + 1                                                     PACKER 470
      READ (SOLWTH) IWDID,KYR,XLAT,XLONG,NTZ,IRECXO,NOD,CLRNES,DUM,             PACKER 471
     1          IWSOL,IDAT60                                                    PACKER 472
      WRITE (OUTWTH) IWDID,KYR,XLAT,XLONG,NTZ,IRECXO,NOD,CLRNES,TG(IM2),        PACKER 473
     1          IWSOL,IDAT60                                                    PACKER 474
 2550 CONTINUE                                                                  PACKER 475
      RETURN                                                                    PACKER 476
 2600 DO 2650 IM=1,24                                                           PACKER 477
      IM2    = (IM-1)/2 + 1                                                     PACKER 478
      READ (SOLWTH) IWDID,KYR,XLAT,XLONG,NTZ,IRECXO,NOD,CLRNES,DUM,             PACKER 479
     1          IWSOL,IDAT30                                                    PACKER 480
      WRITE (OUTWTH) IWDID,KYR,XLAT,XLONG,NTZ,IRECXO,NOD,CLRNES,TG(IM2),        PACKER 481
     1          IWSOL,IDAT30                                                    PACKER 482
 2650 CONTINUE                                                                  PACKER 483
      RETURN                                                                    PACKER 484
 3100 STOPIT = .TRUE.                                                           PACKER 485
      WRITE(OUTFIL,6001)                                                        PACKER 486
      RETURN                                                                    PACKER 487
 3200 WRITE(OUTFIL,6002)                                                        PACKER 488
      RETURN                                                                    PACKER 489
C              END OF FILE READ                                                 PACKER 490
 3400 WRITE(OUTFIL,8005) NAMETT(1,ITPTYP+1), NAMETT(2,ITPTYP+1)                 PACKER 491
      STOPIT = .TRUE.                                                           PACKER 492
      RETURN                                                                    PACKER 493
C              INTERPOLATION INTERVAL EXCEEDED                                  PACKER 494
 3500 WRITE(OUTFIL,8003)                                                        PACKER 495
      WRITE(OUTFIL,8004)  ((NAMES(I,J),I=1,3),J=1,8),                           PACKER 496
     1             ((IRAW(I,J),J=1,8),I=1,24)                                   PACKER 497
      STOPIT = .TRUE.                                                           PACKER 498
      RETURN                                                                    PACKER 499
      END                                                                       PACKER 500
      FUNCTION PPWVMS(TEMP)                                                     PPWVMS   2
C                                                                               PPWVMS   3
C              CALCULATES THE VAPOR PRESSURE OF AIR                             PPWVMS   4
C                                                                               PPWVMS   5
C                                                                               PPWVMS   6
      DIMENSION  A(6)                                                           PPWVMS   7
      DIMENSION  B(4)                                                           PPWVMS   8
C                                                                               PPWVMS   9
      DATA  A        /-7.90298,5.02808,-1.3816E-7,11.344,8.1328E-3,             PPWVMS  10
     1               -3.49149/                                                  PPWVMS  11
      DATA  B        /-9.09718,-3.56654,0.876793,0.0060273/                     PPWVMS  12
C                                                                               PPWVMS  13
      T      = (TEMP+459.7)/1.8                                                 PPWVMS  14
      IF (T .LT. 273.16) GOTO 3000                                              PPWVMS  15
      Z      = 373.16/T                                                         PPWVMS  16
      P1     = A(1)*(Z-1.0)                                                     PPWVMS  17
      P2     = A(2)*ALOG10(Z)                                                   PPWVMS  18
      P3     = A(3)*(10.0**(A(4)*(1.0-1.0/Z))-1.0)                              PPWVMS  19
      P4     = A(5)*(10.0**(A(6)*(Z-1.0))-1.0)                                  PPWVMS  20
      GOTO 3010                                                                 PPWVMS  21
 3000 CONTINUE                                                                  PPWVMS  22
      Z      = 273.16/T                                                         PPWVMS  23
      P1     = B(1)*(Z-1.0)                                                     PPWVMS  24
      P2     = B(2)*ALOG10(Z)                                                   PPWVMS  25
      P3     = B(3)*(1.0-1.0/Z)                                                 PPWVMS  26
      P4     = ALOG10(B(4))                                                     PPWVMS  27
 3010 CONTINUE                                                                  PPWVMS  28
      PPWVMS = 29.921*(10.0**(P1+P2+P3+P4))                                     PPWVMS  29
      RETURN                                                                    PPWVMS  30
      END                                                                       PPWVMS  31
      FUNCTION PVCALC(DBT,WBT,PATM)                                             PVCALC   2
C                                                                               PVCALC   3
C              CALCULATES THE VAPOR PRESSURE USING DRYBULB TEMPERATURE,         PVCALC   4
C              WETBULB TEMPERATURE, AND ATMOSPHERIC PRESSURE ARE GIVEN          PVCALC   5
C                                                                               PVCALC   6
      PVP    = PPWVMS(WBT)                                                      PVCALC   7
      IF(DBT .LE. WBT) GOTO 3000                                                PVCALC   8
      WSTAR  = 0.622*PVP/(PATM-PVP)                                             PVCALC   9
      IF(WBT .GT. 32.0) GOTO 3010                                               PVCALC  10
      PVCALC = PVP - 5.704E-4*PATM*(DBT-WBT)/1.8                                PVCALC  11
      GOTO 3020                                                                 PVCALC  12
 3000 CONTINUE                                                                  PVCALC  13
      PVCALC = PVP                                                              PVCALC  14
      GOTO 3020                                                                 PVCALC  15
 3010 CONTINUE                                                                  PVCALC  16
      CDB    = (DBT-32.0)/1.8                                                   PVCALC  17
      CWB    = (WBT-32.0)/1.8                                                   PVCALC  18
      HL     = 597.31 + 0.4409*CDB - CWB                                        PVCALC  19
      CH     = 0.2402 + 0.4409*WSTAR                                            PVCALC  20
      EX     = (WSTAR - CH*(CDB-CWB)/HL)/0.622                                  PVCALC  21
      PVCALC = PATM*EX/(1.0+EX)                                                 PVCALC  22
 3020 CONTINUE                                                                  PVCALC  23
      RETURN                                                                    PVCALC  24
      END                                                                       PVCALC  25
      FUNCTION RELHUM(DBT,WBT,PATM)                                             RELHUM   2
C                                                                               RELHUM   3
C              CALCULATES RELATIVE HUMIDITY GIVEN DRYBULB/WETBULB               RELHUM   4
C              TEMPERATURES AND ATMOSPHERIC PRESSURE                            RELHUM   5
C                                                                               RELHUM   6
C              'PV' = VAPOR PRESSURE                                            RELHUM   7
C                                                                               RELHUM   8
      PV     = PVCALC(DBT,WBT,PATM)                                             RELHUM   9
      PVSAT  = PPWVMS(DBT)                                                      RELHUM  10
      RELHUM = PV/PVSAT*100.0                                                   RELHUM  11
      RETURN                                                                    RELHUM  12
      END                                                                       RELHUM  13
      SUBROUTINE REPORT                                                         REPORT   2
C                                                                               REPORT   3
C              THIS ROUTINE LISTS THE CONTENTS OF A PACKED DOE-N WEATHER        REPORT   4
C              TAPE FILE ON THE FILE OUTPUT IN HOURLY BLOCKS                    REPORT   5
C                                                                               REPORT   6
C              ---VARIABLES---                                                  REPORT   7
C                                                                               REPORT   8
C              'AVG'     = SUMMARY OF MONTHLY VALUES (SEE BELOW)                REPORT   9
C              'CLMNTH'  = SUMMARY OF CLOUD AMOUNTS BY MONTH                    REPORT  10
C              'IBEGM'   = STARTING MONTH FOR REPORT                            REPORT  11
C              'ID'      = DAY OF MONTH INDEX                                   REPORT  12
C              'IDATE'   = HOLERITH REPRESENTATION OF DATE                      REPORT  13
C              'IDAY'    = COMMON BLOCK VALUE OF DAY OF MONTH                   REPORT  14
C              'IENDM'   = ENDING MONTH FOR REPORT                              REPORT  15
C              'IH'      = HOUR OF DAY INDEX                                    REPORT  16
C              'IHOUR'   = COMMON BLOCK VALUE OF HOUR OF DAY                    REPORT  17
C              'IM'      = MONTH OF YEAR INDEX                                  REPORT  18
C              'IMNTH'   = COMMON BLOCK VALUE OF MONTH OF YEAR                  REPORT  19
C              'ITIME'   = HOLERITH REPRESENTATION OF TIME                      REPORT  20
C              'MNAMES'  = NAMES OF MONTH ARRAY                                 REPORT  21
C              'MDAYS'   = NUMBER OF DAYS BY MONTH                              REPORT  22
C              'NHRN'    = NUMBER OF HOURS RAIN BY MONTH                        REPORT  23
C              'NHSN'    = NUMBER OF HOURS SNOW BY MONTH                        REPORT  24
C              'NOD'     = NUMBER OF DAYS IN MONTH                              REPORT  25
C              'INWTH'   = DEVICE VARIABLE FOR INPUT WEATHER                    REPORT  26
C              'TGMNTH'  = SUMMARY OF GROUND TEMPERATURES BY MONTH              REPORT  27
C                                                                               REPORT  28
C              ---ARRAY ELEMENTS---                                             REPORT  29
C                                                                               REPORT  30
C              AVG(1,1,IH) = MIN WET BULB TEMP IN MONTH                         REPORT  31
C                  2       = MAX                                                REPORT  32
C                  3       = AVERAGE                                            REPORT  33
C                  1,2     = MIN DRY BULB TEMP IN MONTH                         REPORT  34
C                  2       = MAX                                                REPORT  35
C                  3       = AVERAGE                                            REPORT  36
C                  1,3     = MIN HUMIDITY RATIO IN MONTH                        REPORT  37
C                  2       = MAX                                                REPORT  38
C                  3       = AVERAGE                                            REPORT  39
C                  1,4     = MIN CLOUD AMOUNT IN MONTH                          REPORT  40
C                  2       = MAX                                                REPORT  41
C                  3       = AVERAGE                                            REPORT  42
C                  1,5     = MIN WIND SPEED IN MONTH                            REPORT  43
C                  2       = MAX                                                REPORT  44
C                  3       = AVERAGE                                            REPORT  45
C              ...AND, IF SOLAR DATA HAS BEEN PACKED...                         REPORT  46
C                  1,6     = TOTAL HORIZONTAL RADIATION                         REPORT  47
C                  2       = DIRECT NORMAL RADIATION                            REPORT  48
C                                                                               REPORT  49
C              ---EXTERNALS---                                                  REPORT  50
C                                                                               REPORT  51
C              'EOF'     RETURNS A NON-ZERO VALUE IF AN END-OF-FILE OR          REPORT  52
C                   RECORD HAS BEEN REACHED (SYSTEM)                            REPORT  53
C              'DATE'    RETURNS HOLORITH DATE, E.G. ' 31 APR 78'               REPORT  54
C                        (SYSTEM)                                               REPORT  55
C              'TIME'    RETURNS HOLORITH TIME, E.G. '  17.34.23'               REPORT  56
C              'WDREAD'  READS WEATHER VALUES FROM DOE-N PACKED WEATHER         REPORT  57
C                        (SYSTEM)                                               REPORT  58
C                   FILES USING TIME VALUES IN /DATE/ AND PLACES THEM           REPORT  59
C                   IN /WEATHD/                                                 REPORT  60
C                                                                               REPORT  61
C                                                                               REPORT  62
      COMMON  /CONST/    DTOR, PIOVR2, PIOVR4, PIOV12, IBLNK                    /CONST/  2
C                                                                               /CONST/  3
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /FILTYP/   IFTYP,IWSZ,ITPTYP                                      /FILTYP/ 2
C                                                                               /FILTYP/ 3
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /LOCALD/   STALAT,STALON,SSTALA,CSTALA,TSTALA,HRSLON              /LOCALD/ 2
C                                                                               /LOCALD/ 3
      COMMON  /MONTHC/   BEFORE(12),MDAYS(12),MNAMES(12)                        /MONTHC/ 2
      INTEGER            BEFORE                                                 /MONTHC/ 3
C                                                                               /MONTHC/ 4
      COMMON  /PACKEC/   NTZ,ISTAT,XLAT,XLONG,                                  /PACKEC/ 2
     1                   IYR,INTINT,DDBT,IBEGH,NBS,IDOYSH(5),ISSHFT(5),         /PACKEC/ 3
     2                   ISHFT,ALT,SKYCOV(12),ILOC                              073a     1
C                                                                               /PACKEC/ 5
      COMMON  /PARAMS/   STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 2
      LOGICAL            STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 3
C                                                                               /PARAMS/ 4
      COMMON  /PCKINT/   KYR,KSTAT,CN(12),TG(12),KTIM,KARD(500),KYRL            -089     1
C                                                                               /PCKINT/ 3
      COMMON  /RAWDAT/   IDRY(24),IWET(24),IDEW(24),IPRESS(24),                 /RAWDAT/ 2
     1                   IWNDSP(24),ICLAMT(24),ISOL(24),IDN(24),                /RAWDAT/ 3
     2                   IWNDIR(24),ICLTY(24),IRN(24),ISN(24),                  CD144S   1
     3                   IOPQCA(24),IRELH(24),ICLTY1(24)                        CD144S   2
      DIMENSION IRAW(24,14)                                                     CD144S   3
      EQUIVALENCE        (IDRY(1),IRAW(1,1))                                    /RAWDAT/ 6
      COMMON /LSTIME/   LSTHRS(24)                                              /RAWDAT/ 7
C                                                                               /RAWDAT/ 8
      COMMON  /REPORC/   IBEGM,IENDM                                            /REPORC/ 2
C                                                                               /REPORC/ 3
      COMMON  /SKIPPY/   FIRSTS,NCALL                                           /SKIPPY/ 2
      LOGICAL            FIRSTS                                                 /SKIPPY/ 3
C                                                                               /SKIPPY/ 4
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
      COMMON  /WEATHD/   IWDID(5),FIRSTW,LRECX,IWSOL,WLONG,WLAT,IWTZN,          /WEATHD/ 2
     1                   CLRNES,TGRND,IWYR,WBT,DBT,PATM,CLDAMT,ISNOW,           /WEATHD/ 3
     2                   IRAIN,IWNDDR,HUMRAT,DENSTY,ENTHAL,SOLRAD,              /WEATHD/ 4
     3                   DIRSOL,ICLDTY,WNDSPD,IRFCP,DPT                         /WEATHD/ 5
      LOGICAL FIRSTW                                                            /WEATHD/ 6
      DIMENSION          CALC(16)                                               /WEATHD/ 7
      EQUIVALENCE        (WBT,CALC(1))                                          /WEATHD/ 8
C                                                                               /WEATHD/ 9
      integer      ITIME(3), IDATE(3)                                           REPORT  77
        CHARACTER*12         CTIME, CDATE                                       WTHWF   20
          EQUIVALENCE      ( IDATE, CDATE ),( ITIME, CTIME )                    WTHWF   21
      DIMENSION  AVG(3,6,12),IAVG(3,6,12)                                       REPORT  80
      DIMENSION  CLMNTH(12)                                                     REPORT  81
      DIMENSION  NAMETT(2,17), ITAPE(2)                                         -089    63
      DIMENSION  NAMS(4,8)                                                      REPORT  84
      DIMENSION  NHRN(12)                                                       REPORT  85
      DIMENSION NHRS(5)                                                         REPORT  86
      DIMENSION  NHSN(12)                                                       REPORT  87
      DIMENSION  TGMNTH(12)                                                     REPORT  89
C                                                                               REPORT  90
      EQUIVALENCE  (IAVG(1,1,1),AVG(1,1,1))                                     REPORT  91
C                                                                               REPORT  92
      LOGICAL  EVEN                                                             REPORT  93
      LOGICAL ONEYEAR                                                           -089    64
C                                                                               REPORT  94
      DATA AVG       /216*0.0/                                                  REPORT  95
      DATA IIFLAG    /1/                                                        REPORT  96
      DATA  NAMETT    /4HPACK,2HED,4HOTHE,2HR ,4HTRY ,2H  ,4H1440,2H  ,         REPORT  97
     1                4H1440,2H-3,4HCTZ ,2H  ,4HTMY ,2H  ,4HSOLM,2HET,          REPORT  98
     2                4HWYEC,2H  ,4HTRYK,2HS ,4HCD14,2H4 ,4HCD14,2H43,          REPORT  99
     3                4HTD96,2H85,4HCANA,2HDA,4HWYEC,2H2 ,4HTRYS,2HLM,          -089    65
     4                4HTMY2,2H                                                 -089    66
     .                /                                                         REPORT 101
      DATA  NAMS     /4HFILE,4H TYP,4HE   ,4H    ,                              REPORT 103
     1                4HYEAR,4H    ,4H    ,4H    ,                              REPORT 104
     2                4HSTAT,4HION ,4HNUMB,4HER  ,                              REPORT 105
     3                4HBEGI,4HNNIN,4HG MO,4HNTH ,                              REPORT 106
     4                4HENDI,4HNG M,4HONTH,4H    ,                              REPORT 107
     5                4HTIME,4H ZON,4HE   ,4H    ,                              REPORT 108
     6                4HLATI,4HTUDE,4H    ,4H    ,                              REPORT 109
     7                4HLONG,4HITUD,4HE   ,4H    /                              REPORT 110
      DATA  NHSN     /12*0/                                                     REPORT 111
      DATA  NHRN     /12*0/                                                     REPORT 112
C                                                                               REPORT 113
 5000 FORMAT(A4,A2,5I6,2F6.1)                                                   REPORT 114
 5001 FORMAT(1X,I5)                                                             -089    67
 6000 FORMAT(////1H ,14H--- ERROR --- ,29HNOT ENOUGH INPUT FOR RUN TYPE,        REPORT 115
     1       29H REP.  PROCESSING TERMINATED.)                                  REPORT 116
 6001 FORMAT(////1H ,36HPROCESSING ENDED DUE TO INPUT ERRORS)                   REPORT 117
 6002 FORMAT(1H ,14H--- ERROR --- ,33HEND OF FILE READ ON WEATHER FILE.,        REPORT 118
     1       24H  PROCESSING TERMINATED.)                                       REPORT 119
 7000 FORMAT( 7H1CITY-  ,5A4,67X,'DOE-2.2',A4,2X,                               VER22    2
     1        3A4, 1X, 3A4)                                                     WTHe     4
 7010 FORMAT( 4H LAT ,F7.1, 6H  LONG ,F7.1,3X, 2HTZ ,I3)                        REPORT 122
 7020 FORMAT(1H ,11HREPORT- W01,2X,A4,A2,2X,23HWEATHER FILE SUMMARY - ,         REPORT 123
     1       A3,6H THRU ,A3,10X,7HSTATION,1X,I5,4X,4HYEAR,1X,I4)                REPORT 124
 7030 FORMAT(1X,130( 1H- ))                                                     REPORT 125
 7040 FORMAT(2X,5( 1H- ),A3,I3,5( 1H- )/5X,                                     REPORT 126
     1 55H HOUR    WBT    DBT  PRESS  CLOUDS  SNOW  RAIN  WNDDIR  ,             REPORT 127
     2 52H HUMID  DENSITY  ENTH  THZSOL  DNMSOL  CTYPE  WSPEED  /               REPORT 128
     3 5X,55H ----  -----  -----  -----  ------  ----  ----  ------  ,          REPORT 129
     4       52H -----  ------  ----  ------  ------  -----  ------  )          REPORT 130
 7050 FORMAT(5X,I5,2F7.0,F7.1,F8.0,2I6,I8,F7.4,F8.3,F6.1,2F8.1,I7,F8.0)         REPORT 131
 7051 FORMAT(5X,I5,2F7.0,F7.1,F8.0,2I6,I8,F7.4,F8.3,F6.1,2F8.1,7X,F8.0)         REPORT 132
 7052 FORMAT(5X,I5,2F7.0,F7.1,F8.0,2I6,8X,F7.4,F8.3,F6.1,2F8.1,I7,F8.0)         REPORT 133
 7053 FORMAT(5X,I5,2F7.0,F7.1,F8.0,2I6,8X,F7.4,F8.3,F6.1,2F8.1,7X,F8.0)         REPORT 134
 7060 FORMAT(13( 1H0 /)/)                                                       REPORT 135
 7070 FORMAT(1H0/19H MONTHLY SUMMARY - ,5A4,2H, ,I5)                            REPORT 136
 7080 FORMAT( 1H0 /                                                             REPORT 137
     1       46H       NO.   GROUND  CLEAR-  -------WBT------  ,                REPORT 138
     2       54H -------DBT------  ------HUMID-----  --CLOUD AMOUNT--  ,        REPORT 139
     3       29H ---WIND SPEED---   HRS   HRS /                                 REPORT 140
     4       46H MONTH DYS     TEMP    NESS   MIN   MAX   AVG  ,                REPORT 141
     5       54H  MIN   MAX   AVG   MIN   MAX   AVG   MIN   MAX   AVG  ,        REPORT 142
     6       29H  MIN   MAX   AVG  SNOW  RAIN )                                 REPORT 143
 7090 FORMAT(46H ----- ---   ------  ------  ----  ----  ----  ,                REPORT 144
     1       54H ----  ----  ----  ----  ----  ----  ----  ----  ----  ,        REPORT 145
     2       29H ----  ----  ----  ----  ---- )                                 REPORT 146
 7100 FORMAT(3X,A3,I4,F9.1,F8.2,6F6.0,3(1X,F5.4),6F6.0,2I6)                     REPORT 147
 7110 FORMAT(1X)                                                                REPORT 148
 7120 FORMAT(33(1H /)/)                                                         REPORT 149
 7140 FORMAT(1H ,1X,5(1H-),A3,I3,5(1H-)/1H ,6X,4HHOUR,7X,3HDBT,7X,              REPORT 150
     1       3HWBT,7X,3HDPT,5X,5HPRESS,4X,6HWSPEED,4X,6HWNDDIR,4X,              REPORT 151
     2       6HCLOUDS,5X,5HCTYPE,6X,4HRAIN,6X,4HSNOW,4X,6HTHZSOL,               REPORT 152
     3       4X,6HDNMSOL/1H ,6X,4H----,7X,3H---,7X,3H---,7X,3H---,5X,           REPORT 153
     4       5H-----,4X,6H------,4X,6H------,4X,6H------,5X,5H-----,            REPORT 154
     5       6X,4H----,6X,4H----,4X,6H------,4X,6H------)                       REPORT 155
 7150 FORMAT(1H ,6X,I4,12I10)                                                   REPORT 156
 7170 FORMAT(//1H ,18HMONTHLY SUMMARY - ,5X,7HSTATION,1X,I5,                    REPORT 157
     1       4X,4HYEAR,1X,I4)                                                   REPORT 158
 7180 FORMAT(//1H ,15X,19H-------DBT-------- ,19H-------WBT-------- ,           REPORT 159
     1       19H-------DPT-------- ,19H---CLOUD AMOUNT--- ,                     REPORT 160
     2       18H----WIND SPEED----,4X,3HHRS,4X,3HHRS/1H ,5HMONTH,               REPORT 161
     3       1X,3HDYS,4X,5(4X,3HMIN,3X,3HMAX,3X,3HAVG),4X,4HRAIN,               REPORT 162
     4       3X,4HSNOW)                                                         REPORT 163
 7190 FORMAT(1H ,5H-----,1X,3H---,4X,5(4X,3H---,3X,3H---,3X,3H---),             REPORT 164
     1       4X,4H----,3X,4H----)                                               REPORT 165
 7200 FORMAT(1H ,2X,A3,I4,4X,5(I7,I6,F6.1),I8,I7)                               REPORT 166
 7240 FORMAT(1H ,1X,5(1H-),A3,I3,5(1H-)/1H ,6X,4HHOUR,7X,3HDBT,7X,3HWBT,        REPORT 167
     1       7X,3HDPT,5X,5HPRESS,4X,6HWSPEED,4X,6HWNDDIR,4X,6HCLOUDS,           REPORT 168
     2       5X,5HCTYPE,4X,6HTOTSOL,4X,6HDIRSOL,4X,6HDIFSOL,6X,4HSALT/          REPORT 169
     3       1H ,6X,4H----,3(7X,3H---),5X,5H-----,3(4X,6H------),5X,            REPORT 170
     4       5H-----,3(4X,6H------),6X,4H----)                                  REPORT 171
C                                                                               REPORT 172
      IF (IIFLAG .EQ. 0)  GO TO 3                                               REPORT 173
      DO 2 KK=1,12                                                              REPORT 174
      DO 1 JJ=1,6                                                               REPORT 175
    1 AVG(1,JJ,KK) = 1000.                                                      REPORT 176
    2 CONTINUE                                                                  REPORT 177
      IIFLAG = 0                                                                REPORT 178
    3 CONTINUE                                                                  REPORT 179
C              READ TAPE TYPE, YEAR, STATION, BEG AND END MONTH                 REPORT 180
      READ(INFIL,5000,END=3100)  ITAPE,IYR,ISTAT,IBEGM,IENDM,                   REPORT 181
     .                           NTZ,XLAT,XLONG                                 REPORT 182
C              ECHO OUT AND CHECK FOR ERRORS                                    REPORT 183
      CALL VALIDA(ITAPE(1),NAMETT(1,1),17,NAMS(1,1),STOPIT,ITPTYP)              -089    68
      CALL VALIDI(IYR,-1,1999,NAMS(1,2),0,STOPIT)                               REPORT 185
      CALL VALIDI(ISTAT,0,99999,NAMS(1,3),0,STOPIT)                             REPORT 186
      CALL VALIDI(IBEGM,1,12,NAMS(1,4),0,STOPIT)                                REPORT 187
      CALL VALIDI(IENDM,1,12,NAMS(1,5),0,STOPIT)                                REPORT 188
      IF (STOPIT)  GO TO 3200                                                   REPORT 189
      ITPX   = ITPTYP                                                           REPORT 190
      IF (.NOT.NEWPAK) THEN                                                     WTHWF   22
        IF (ITPX .EQ. 1)  THEN                                                  WTHWF   23
          OPEN ( 10, FILE='WEATHR.TMP', FORM='UNFORMATTED'                      WTHWF   24
     $                                , STATUS='UNKNOWN' ,                      WTHWF   25
     $      recordtype='variable',recl=6200,blocksize=148992)                   WTHWF   26
        ELSEIF (ITPX .GT. 1) THEN                                               WTHWF   27
          OPEN ( 10, FILE='WEATHR.TMP', FORM='FORMATTED'                        WTHWF   28
     $                                , STATUS='UNKNOWN' ,                      WTHWF   29
     $                            recl=1024,blocksize=132000)                   WTHWF   30
        ENDIF                                                                   WTHWF   31
      ENDIF                                                                     WTHWF   32
      ITPTYP = ITPTYP - 1                                                       REPORT 197
      IF ((ITPTYP .EQ. 4) .OR. (ITPTYP .EQ. 6) .OR. (ITPTYP .EQ.7)              REPORT 198
     1 .OR. (ITPTYP .EQ.8) .OR. (ITPTYP .EQ. 9) .OR. (ITPTYP .EQ. 11))          REPORT 199
     2 CALL VALIDI(NTZ,-12,12,NAMS(1,6),0,STOPIT)                               REPORT 200
      IBEGH = 1                                                                 073b    83
      IF ((ITPTYP .EQ. 4) .OR. (ITPTYP .EQ. 11))                                REPORT 201
     1   IBEGH = ((IABS(NTZ)+2)/3)*3 - IABS(NTZ) + 1                            073b    84
      IF ((ITPTYP .LT. 6) .OR. (ITPTYP .GT. 9)) GO TO 4                         REPORT 203
      CALL VALIDF(XLAT,0.,90.,NAMS(1,7),0,STOPIT)                               REPORT 204
      CALL VALIDF(XLONG,-180.,180.,NAMS(1,8),0,STOPIT)                          REPORT 205
      IF (STOPIT)  GO TO 3200                                                   REPORT 206
      STALAT = DTOR * XLAT                                                      LOCALC   2
      STALON = DTOR * XLONG                                                     LOCALC   3
      SSTALA = SIN(STALAT)                                                      LOCALC   4
      CSTALA = COS(STALAT)                                                      LOCALC   5
      TSTALA = SSTALA/CSTALA                                                    LOCALC   6
      HRSLON = XLONG/15.                                                        LOCALC   7
    4 CONTINUE                                                                  REPORT 208
      ALT = 0.                                                                  WTH2    43
      IF (ISTAT.NE.-999) KSTAT=ISTAT                                            -089    69
      KYR = IYR                                                                 -089    70
      IF (ITPTYP.EQ.16) THEN                                                    -089    71
          READ (INWTH,5001,END=3300) KSTAT                                      -089    72
      END IF                                                                    -089    73
C              SET AVG ARRAY                                                    REPORT 209
      DO 5 K = 1,12                                                             REPORT 210
      NHSN(K) = 0                                                               REPORT 211
      NHRN(K) = 0                                                               REPORT 212
      SKYCOV(K) = 0.                                                            -092     6
      DO 5 J = 1,6                                                              REPORT 213
      AVG(1,J,K) = 999.                                                         REPORT 214
      AVG(2,J,K) = -999.                                                        REPORT 215
      AVG(3,J,K) = 0.                                                           REPORT 216
    5 CONTINUE                                                                  REPORT 217
      IF (ITPTYP .EQ. 0)  GO TO 9                                               REPORT 218
      DO 7 K = 1,12                                                             REPORT 219
      DO 7 J = 1,6                                                              REPORT 220
      IAVG(1,J,K) = 999                                                         REPORT 221
      IAVG(2,J,K) = -999                                                        REPORT 222
    7 CONTINUE                                                                  REPORT 223
C                                                                               REPORT 224
C              GET DATE AND TIME                                                REPORT 225
    9 CALL TIMDAT( CTIME, CDATE )                                               WTHWF   33
      IF ((ITPTYP .EQ. 0) .AND. (.NOT. NEWPAK))  REWIND  INWTH                  REPORT 227
C                                                                               REPORT 228
C              INITIALIZE HEADER PARAMETERS                                     REPORT 229
      IMNTH  = 1                                                                REPORT 230
      IDAY   = 1                                                                REPORT 231
      IHOUR  = 1                                                                REPORT 232
      NCALL = 0                                                                 WTH2    45
      FIRSTS = .TRUE.                                                           WTH2    46
      ONEYEAR = .TRUE.                                                          -089    74
C                                                                               REPORT 247
C              LOOP THROUGH MONTHS                                              REPORT 248
      DO 2000 IM = IBEGM,IENDM                                                  REPORT 249
      IMNTH  = IM                                                               REPORT 250
      NOD    = MDAYS(IM)                                                        REPORT 251
      EVEN   = .TRUE.                                                           REPORT 252
C              ZERO OUT HOURS                                                   REPORT 253
      DO 10 I=1,5                                                               REPORT 254
   10 NHRS(I) = 0                                                               REPORT 255
C                                                                               REPORT 256
C              LOOP THROUGH DAYS                                                REPORT 257
      DO 1000 ID = 1,NOD                                                        REPORT 258
      IDAY   = ID                                                               REPORT 259
      EVEN   = .NOT. EVEN                                                       REPORT 262
C                                                                               REPORT 277
C              LOOP THROUGH HOURS                                               REPORT 278
      IF (ITPTYP .NE. 0)  GO TO 200                                             REPORT 279
      DO 100 IH = 1,24                                                          REPORT 280
      IHOUR  = IH                                                               REPORT 281
C                                                                               REPORT 282
C              GET WEATHER VAULES                                               REPORT 283
      CALL WDREAD                                                               REPORT 284
      IF (.NOT.FIRSTS .AND. KYR.NE.IWYR) ONEYEAR=.FALSE.                        -089    75
      KYR = IWYR                                                                -089    76
      FIRSTS = .FALSE.                                                          -089    77
      IF (IEOF)  GO TO 3300                                                     REPORT 285
C                                                                               -089    78
C              PRINT PAGE HEADER ON ODD DAYS                                    -089    79
      IF (IH.EQ.1) THEN                                                         -089    80
          IF (.NOT.EVEN) THEN                                                   -089    81
              WRITE(OUTFIL,7000)IWDID,VERS,IDATE,ITIME                          -089    82
              WRITE(OUTFIL,7010)WLAT,WLONG,IWTZN                                -089    83
              WRITE(OUTFIL,7020)NAMETT(1,ITPX),NAMETT(2,ITPX),                  -089    84
     1                          MNAMES(IBEGM),MNAMES(IENDM),KSTAT,KYR           -089    85
              WRITE(OUTFIL,7030)                                                -089    86
          ELSE                                                                  -089    87
              WRITE(OUTFIL,7030)                                                -089    88
          END IF                                                                -089    89
          WRITE(OUTFIL,7040) MNAMES(IM),ID                                      -089    90
      END IF                                                                    -089    91
C                                                                               REPORT 286
C              SET 'AVG' VALUES                                                 REPORT 287
      AVG(1,1,IM) = AMIN1(WBT,AVG(1,1,IM))                                      REPORT 288
      AVG(2,1,IM) = AMAX1(WBT,AVG(2,1,IM))                                      REPORT 289
      AVG(3,1,IM) = AVG(3,1,IM) + WBT                                           REPORT 290
      AVG(1,2,IM) = AMIN1(DBT,AVG(1,2,IM))                                      REPORT 291
      AVG(2,2,IM) = AMAX1(DBT,AVG(2,2,IM))                                      REPORT 292
      AVG(3,2,IM) = AVG(3,2,IM) + DBT                                           REPORT 293
      AVG(1,3,IM) = AMIN1(HUMRAT,AVG(1,3,IM))                                   REPORT 294
      AVG(2,3,IM) = AMAX1(HUMRAT,AVG(2,3,IM))                                   REPORT 295
      AVG(3,3,IM) = AVG(3,3,IM) + HUMRAT                                        REPORT 296
      AVG(1,4,IM) = AMIN1(CLDAMT,AVG(1,4,IM))                                   REPORT 297
      AVG(2,4,IM) = AMAX1(CLDAMT,AVG(2,4,IM))                                   REPORT 298
      AVG(3,4,IM) = AVG(3,4,IM) + CLDAMT                                        REPORT 299
      AVG(1,5,IM) = AMIN1(WNDSPD,AVG(1,5,IM))                                   REPORT 300
      AVG(2,5,IM) = AMAX1(WNDSPD,AVG(2,5,IM))                                   REPORT 301
      AVG(3,5,IM) = AVG(3,5,IM) + WNDSPD                                        REPORT 302
      AVG(1,6,IM) = AVG(1,6,IM) + SOLRAD                                        REPORT 303
      AVG(2,6,IM) = AVG(2,6,IM) + DIRSOL                                        REPORT 304
C                                                                               REPORT 305
C              CHECK FOR SNOW AND RAIN                                          REPORT 306
      IF (ISNOW .NE. 0) NHSN(IM) = NHSN(IM) + 1                                 REPORT 307
      IF (IRAIN .NE. 0) NHRN(IM) = NHRN(IM) + 1                                 REPORT 308
C                                                                               REPORT 309
C              PRINT HOURLY VALUES                                              REPORT 310
      IF ((CLDAMT .GT. 0.) .AND. (WNDSPD .GT. 0.))                              REPORT 311
     1 WRITE(OUTFIL,7050,ERR=99) IH,WBT,DBT,PATM,CLDAMT,ISNOW,IRAIN,            -092     7
     2   IWNDDR,HUMRAT,DENSTY,ENTHAL,SOLRAD,DIRSOL,ICLDTY,WNDSPD                -092     8
   99 CONTINUE                                                                  REPORT 313
      IF ((CLDAMT .LE. 0.) .AND. (WNDSPD .GT. 0.))                              REPORT 314
     1 WRITE(OUTFIL,7051,ERR=100)IH,WBT,DBT,PATM,CLDAMT,ISNOW,IRAIN,            -092     9
     2   IWNDDR,HUMRAT,DENSTY,ENTHAL,SOLRAD,DIRSOL,WNDSPD                       -092    10
      IF ((CLDAMT .GT. 0.) .AND. (WNDSPD .LE. 0.))                              REPORT 316
     1 WRITE(OUTFIL,7052,ERR=100)IH,WBT,DBT,PATM,CLDAMT,ISNOW,IRAIN,            -092    11
     2   HUMRAT,DENSTY,ENTHAL,SOLRAD,DIRSOL,ICLDTY,WNDSPD                       -092    12
      IF ((CLDAMT .LE. 0.) .AND. (WNDSPD .LE. 0.))                              REPORT 318
     1 WRITE(OUTFIL,7053,ERR=100)IH,WBT,DBT,PATM,CLDAMT,ISNOW,IRAIN,            -092    13
     2   HUMRAT,DENSTY,ENTHAL,SOLRAD,DIRSOL,WNDSPD                              -092    14
  100 CONTINUE                                                                  REPORT 321
      DO 110 I=1,5                                                              REPORT 322
  110 NHRS(I) = NOD*24                                                          REPORT 323
      GO TO 400                                                                 REPORT 324
  200 CONTINUE                                                                  REPORT 325
C              READ IN 1 DAY OF WEATHER DATA FROM TAPE                          REPORT 326
      GO TO (210,220,230,230,240,245,245,220,220,240,240,247,                   WTH2    50
     1       248,249,220,2491),ITPTYP                                           -089    92
  210 CALL OTHER                                                                REPORT 328
      GO TO 250                                                                 REPORT 329
  220 CALL TRYDCD                                                               REPORT 330
      GO TO 250                                                                 REPORT 331
  230 CALL WXDCD                                                                REPORT 332
      GO TO 250                                                                 REPORT 333
  240 CALL CTZDCD                                                               REPORT 334
      GO TO 250                                                                 REPORT 335
  245 CALL TMYDCD                                                               REPORT 336
      GO TO 250                                                                 REPORT 337
  247 CALL DC9685                                                               REPORT 338
      GO TO 250                                                                 WTH2    52
  248 CALL CANDCD                                                               WTH2    53
      GO TO 250                                                                 WTH2    54
  249 CALL WY2DCD                                                               WTH2    55
      GO TO 250                                                                 -089    93
 2491 CALL TMY2DCD                                                              -089    94
  250 CONTINUE                                                                  REPORT 339
      IF (IEOF)  GO TO 3300                                                     REPORT 340
      FIRSTS = .FALSE.                                                          -089    95
      IF (.NOT.EVEN) THEN                                                       -089    96
          WRITE(OUTFIL,7000)IWDID,VERS,IDATE,ITIME                              -089    97
          WRITE(OUTFIL,7110)                                                    -089    98
          WRITE(OUTFIL,7020)NAMETT(1,ITPX),NAMETT(2,ITPX),MNAMES(IBEGM),        -089    99
     1                      MNAMES(IENDM),KSTAT,KYR                             -089   100
          WRITE(OUTFIL,7030)                                                    -089   101
      ELSE                                                                      -089   102
          WRITE(OUTFIL,7110)                                                    -089   103
      END IF                                                                    -089   104
      IF(ITPTYP.GT.1) WRITE(OUTFIL,7140) MNAMES(IM),ID                          -089   105
      IF (ITPTYP.EQ.1)  WRITE(OUTFIL,7240) MNAMES(IM),ID                        -089   106
      DO 300 IH=1,24                                                            REPORT 341
      IHOUR  = IH                                                               REPORT 342
      IF (IDRY(IH) .EQ. IUNDEF)  GO TO 260                                      REPORT 343
      NHRS(1) = NHRS(1) + 1                                                     REPORT 344
      IAVG(1,1,IM) = MIN0(IDRY(IH),IAVG(1,1,IM))                                REPORT 345
      IAVG(2,1,IM) = MAX0(IDRY(IH),IAVG(2,1,IM))                                REPORT 346
      IAVG(3,1,IM) = IAVG(3,1,IM) + IDRY(IH)                                    REPORT 347
  260 IF (IWET(IH) .EQ. IUNDEF)  GO TO 270                                      REPORT 348
      NHRS(2) = NHRS(2) + 1                                                     REPORT 349
      IAVG(1,2,IM) = MIN0(IWET(IH),IAVG(1,2,IM))                                REPORT 350
      IAVG(2,2,IM) = MAX0(IWET(IH),IAVG(2,2,IM))                                REPORT 351
      IAVG(3,2,IM) = IAVG(3,2,IM) + IWET(IH)                                    REPORT 352
  270 IF (IDEW(IH) .EQ. IUNDEF)  GO TO 280                                      REPORT 353
      NHRS(3) = NHRS(3) + 1                                                     REPORT 354
      IAVG(1,3,IM) = MIN0(IDEW(IH),IAVG(1,3,IM))                                REPORT 355
      IAVG(2,3,IM) = MAX0(IDEW(IH),IAVG(2,3,IM))                                REPORT 356
      IAVG(3,3,IM) = IAVG(3,3,IM) + IDEW(IH)                                    REPORT 357
  280 IF (ICLAMT(IH) .EQ. IUNDEF)  GO TO 290                                    REPORT 358
      NHRS(4) = NHRS(4) + 1                                                     REPORT 359
      IAVG(1,4,IM) = MIN0(ICLAMT(IH),IAVG(1,4,IM))                              REPORT 360
      IAVG(2,4,IM) = MAX0(ICLAMT(IH),IAVG(2,4,IM))                              REPORT 361
      IAVG(3,4,IM) = IAVG(3,4,IM) + ICLAMT(IH)                                  REPORT 362
  290 IF (IWNDSP(IH) .EQ. IUNDEF)  GO TO 295                                    REPORT 363
      NHRS(5) = NHRS(5) + 1                                                     REPORT 364
      IAVG(1,5,IM) = MIN0(IWNDSP(IH),IAVG(1,5,IM))                              REPORT 365
      IAVG(2,5,IM) = MAX0(IWNDSP(IH),IAVG(2,5,IM))                              REPORT 366
      IAVG(3,5,IM) = IAVG(3,5,IM) + IWNDSP(IH)                                  REPORT 367
  295 IF (ISN(IH) .NE. 0 .AND. ISN(IH) .NE. IUNDEF)                             REPORT 368
     1 NHSN(IM) = NHSN(IM) + 1                                                  REPORT 369
      IF (IRN(IH) .NE. 0 .AND. IRN(IH) .NE. IUNDEF)                             REPORT 370
     1 NHRN(IM) = NHRN(IM) + 1                                                  REPORT 371
      IF ((ITPTYP .LT. 6) .OR. (ITPTYP .GT. 9))  GO TO 299                      REPORT 372
      IAVG(1,6,IM) = IAVG(1,6,IM) + ISOL(IH)                                    REPORT 373
      IAVG(2,6,IM) = IAVG(2,6,IM) + IDN(IH)                                     REPORT 374
  299 IF ((ITPTYP .GT. 1 .AND. ITPTYP .LT. 6) .OR. (ITPTYP .EQ. 10)             REPORT 375
     1   .OR. (ITPTYP .EQ. 11) .OR. (ITPTYP .EQ. 12)                            -089   107
     2   .OR. (ITPTYP .EQ. 15))                                                 -089   108
     2   WRITE (OUTFIL,7150)                                                    REPORT 377
     2            IH,IDRY(IH),IWET(IH),IDEW(IH),                                REPORT 378
     3            IPRESS(IH), IWNDSP(IH), IWNDIR(IH), ICLAMT(IH),               REPORT 379
     4            ICLTY1(IH), IRN(IH), ISN(IH)                                  REPORT 380
      IF (ITPTYP .EQ. 1)  WRITE(OUTFIL,7150) IH, IDRY(IH), IWET(IH),            REPORT 381
     1                                       IDEW(IH),                          REPORT 382
     2                   IPRESS(IH), IWNDSP(IH), IWNDIR(IH), ICLAMT(IH),        REPORT 383
     3                   IUNDEF, ICLTY(IH), IRN(IH), ISN(IH), ICLTY1(IH)        REPORT 384
      IF (ITPTYP.EQ.6 .OR. ITPTYP.EQ.7 .OR. ITPTYP.EQ.8 .OR.                    -089   109
     1    ITPTYP.EQ.9 .OR. ITPTYP.EQ.14 .OR. ITPTYP.EQ.16)                      -089   110
     2    WRITE(OUTFIL,7150)IH,IDRY(IH),IUNDEF,IDEW(IH),IPRESS(IH),             -089   111
     3                      IWNDSP(IH),IWNDIR(IH),ICLAMT(IH),IUNDEF,            -089   112
     4                      IRN(IH),ISN(IH),ISOL(IH),IDN(IH)                    -089   113
  300 CONTINUE                                                                  REPORT 389
  400 CONTINUE                                                                  REPORT 390
C                                                                               REPORT 391
C              PRINT PAGE FOOTER ON EVEN DAYS                                   REPORT 392
      IF (.NOT. EVEN) GOTO 1000                                                 REPORT 393
      WRITE(OUTFIL,7030)                                                        REPORT 394
 1000 CONTINUE                                                                  REPORT 395
C                                                                               REPORT 396
C              STORE CLEARNESS NUMBER AND GROUNG TEMPERATURE FOR MONTH          REPORT 397
      CLMNTH(IM) = CLRNES                                                       REPORT 398
      TGMNTH(IM) = TGRND                                                        REPORT 399
C                                                                               REPORT 400
C              LOOP THROUGH AND DIVIDE TO MAKE AVERAGES                         REPORT 401
      DO 1020 I = 1,5                                                           REPORT 402
      IF (ITPTYP .NE. 0)  AVG(3,I,IM) = FLOAT(IAVG(3,I,IM))                     REPORT 403
      AVG(3,I,IM) = AVG(3,I,IM)/FLOAT(NHRS(I))                                  REPORT 404
 1020 CONTINUE                                                                  REPORT 405
      IF (ITPTYP .EQ. 4)  NHSN(IM) = NHSN(IM)*3                                 REPORT 406
      IF (ITPTYP .EQ. 4)  NHRN(IM) = NHRN(IM)*3                                 REPORT 407
C                                                                               REPORT 408
C              IF ODD NUMBER OF DAYS IN MONTH, PRINT OUT OFFSET SPACE           REPORT 409
C              AND PAGE FOOTER                                                  REPORT 410
      IF (EVEN) GOTO 2000                                                       REPORT 411
      WRITE(OUTFIL,7060)                                                        REPORT 412
      WRITE(OUTFIL,7030)                                                        REPORT 413
 2000 CONTINUE                                                                  REPORT 414
C                                                                               REPORT 415
C              PRINT PAGE HEADER FOR SUMMARY PAGE                               REPORT 416
      WRITE(OUTFIL,7000)IWDID,VERS,IDATE,ITIME                                  REPORT 417
      IF (ITPTYP .EQ. 0)  WRITE(OUTFIL,7010) WLAT, WLONG, IWTZN                 REPORT 418
      IF (ITPTYP .NE. 0)  WRITE(OUTFIL,7110)                                    REPORT 419
      IF (.NOT.ONEYEAR) KYR = -999                                              -089   114
      WRITE(OUTFIL,7020)NAMETT(1,ITPX),NAMETT(2,ITPX),MNAMES(IBEGM),            REPORT 420
     1          MNAMES(IENDM),KSTAT,KYR                                         REPORT 421
      WRITE(OUTFIL,7030)                                                        REPORT 422
C                                                                               REPORT 423
C              PRINT CITY NAME, YEAR AND COLUMN HEADERS                         REPORT 424
      IF (ITPTYP .NE. 0)  GO TO 2020                                            REPORT 425
      WRITE(OUTFIL,7070)IWDID,KYR                                               -089   115
      WRITE(OUTFIL,7080)                                                        REPORT 427
      WRITE(OUTFIL,7090)                                                        REPORT 428
      GO TO 2030                                                                REPORT 429
 2020 WRITE(OUTFIL,7170) KSTAT, KYR                                             REPORT 430
      WRITE(OUTFIL,7180)                                                        REPORT 431
      WRITE(OUTFIL,7190)                                                        REPORT 432
 2030 CONTINUE                                                                  REPORT 433
C                                                                               REPORT 434
C              PRINT OUT MONTHLY VALUES                                         REPORT 435
      DO 2100 IM=IBEGM,IENDM                                                    REPORT 436
      IF (ITPTYP .EQ. 0)  WRITE(OUTFIL,7100) MNAMES(IM), MDAYS(IM),             REPORT 437
     1                                       TGMNTH(IM),                        REPORT 438
     2 CLMNTH(IM), ((AVG(I,J,IM),I=1,3),J=1,5), NHSN(IM), NHRN(IM)              REPORT 439
      IF(ITPTYP.GT.1) WRITE(OUTFIL,7200)MNAMES(IM),MDAYS(IM),                   REPORT 440
     1 (IAVG(1,J,IM), IAVG(2,J,IM), AVG(3,J,IM), J=1,5),                        REPORT 441
     2 NHRN(IM), NHSN(IM)                                                       REPORT 442
      IF (ITPTYP .EQ. 1)  WRITE(OUTFIL,7200) MNAMES(IM), MDAYS(IM),             REPORT 443
     1 (IAVG(1,J,IM), IAVG(2,J,IM), AVG(3,J,IM), J=1,5)                         REPORT 444
 2100 CONTINUE                                                                  REPORT 445
      IDIF   = 11 - IENDM + IBEGM                                               REPORT 446
      IF (IDIF .LE. 0) GOTO 2300                                                REPORT 447
C                                                                               REPORT 448
C              PRINT EXTRA LINES FOR MISSING MONTHS                             REPORT 449
      DO 2200 I = 1,IDIF                                                        REPORT 450
      WRITE(OUTFIL,7110)                                                        REPORT 451
 2200 CONTINUE                                                                  REPORT 452
 2300 CONTINUE                                                                  REPORT 453
C                                                                               REPORT 454
C              PRINT OUT SPACE AND PAGE FOOTER                                  REPORT 455
      WRITE(OUTFIL,7120)                                                        REPORT 456
      WRITE(OUTFIL,7030)                                                        REPORT 457
      RETURN                                                                    REPORT 458
 3100 STOPIT = .TRUE.                                                           REPORT 459
      WRITE(OUTFIL,6000)                                                        REPORT 460
      RETURN                                                                    REPORT 461
 3200 WRITE(OUTFIL,6001)                                                        REPORT 462
      RETURN                                                                    REPORT 463
 3300 STOPIT = .TRUE.                                                           REPORT 464
      WRITE(OUTFIL,6002)                                                        REPORT 465
      RETURN                                                                    REPORT 466
      END                                                                       REPORT 467
      SUBROUTINE  RSWAP(ARRAY,IDIM,I,J)                                         RSWAP    2
C                                                                               RSWAP    3
C              THIS ROUTINE SWAPS THE VALUES IN AN ARRAY                        RSWAP    4
C                                                                               RSWAP    5
C                                                                               RSWAP    6
      DIMENSION  ARRAY(IDIM,336)                                                RSWAP    7
C                                                                               RSWAP    8
      DO 1000 K = 1,IDIM                                                        RSWAP    9
      T     = ARRAY(K,I)                                                        RSWAP   10
      ARRAY(K,I) = ARRAY(K,J)                                                   RSWAP   11
      ARRAY(K,J) = T                                                            RSWAP   12
 1000 CONTINUE                                                                  RSWAP   13
      RETURN                                                                    RSWAP   14
      END                                                                       RSWAP   15
      FUNCTION SLVEXN(GUESS,P1,P2,X,TOL)                                        SLVEXN   2
      COMMON  /TMYME/    IM,KDOM,IH                                             /TMYME/  2
C                                                                               /TMYME/  3
      IF (X .LE. .0)  GO TO 20                                                  SLVEXN   4
      LOOP   = 0                                                                SLVEXN   5
      XLIMIT = 2.67864E-33                                                      SLVEXN   6
      STOL   = ABS(TOL)                                                         SLVEXN   7
      SLVEXN = GUESS                                                            SLVEXN   8
   10 LOOP   = LOOP + 1                                                         SLVEXN   9
      TERM1  = XLIMIT                                                           SLVEXN  10
      IF (SLVEXN*P1 .GT. -75.)  TERM1 = EXP(SLVEXN*P1)                          SLVEXN  11
      TERM2  = XLIMIT                                                           SLVEXN  12
      IF (SLVEXN*P2 .GT. -75.)  TERM2 = EXP(SLVEXN*P2)                          SLVEXN  13
      ERR    = TERM1 + TERM2 - X                                                SLVEXN  14
      TERM1  = P1*TERM1                                                         SLVEXN  15
      TERM2  = P2*TERM2                                                         SLVEXN  16
      DERR   = XLIMIT                                                           SLVEXN  17
      IF ((TERM1+TERM2) .GT. DERR)  DERR = TERM1 + TERM2                        SLVEXN  18
      DDERR  = XLIMIT                                                           SLVEXN  19
      IF (DDERR .LT. (P1*TERM1+P2*TERM2)) DDERR=P1*TERM1+P2*TERM2               SLVEXN  20
      DIVV   = XLIMIT                                                           SLVEXN  21
      IF ((DERR-.5*ERR*DDERR/DERR) .GT. XLIMIT) DIVV =                          SLVEXN  22
     1   DERR - .5*ERR*DDERR/DERR                                               SLVEXN  23
      SLVEXN = SLVEXN - ERR/DIVV                                                SLVEXN  24
      IF (LOOP .GT. 300)  GO TO 20                                              SLVEXN  25
      IF (ABS(ERR).GT.STOL) GO TO 10                                            SLVEXN  26
      RETURN                                                                    SLVEXN  27
C              20 PRINT 9010,IM,KDOM,IH,X                                       SLVEXN  28
   20 SLVEXN = -7.                                                              SLVEXN  29
      RETURN                                                                    SLVEXN  30
 9010 FORMAT(44H TIME-SHIFTING OF TMY SOLAR DATA FAILED, MO=  ,I2,              SLVEXN  31
     1       6H  DAY=,I2,5H  HR=,I2 /                                           SLVEXN  32
     2       44H BECAUSE DIR NRML SOLAR .LE. 0 OR MISSING... ,                  SLVEXN  33
     3       26H2*DIR NRML SOLAR/SOLCON =  ,E12.4)                              SLVEXN  34
      END                                                                       SLVEXN  35
      FUNCTION SLVWBT(PATM,DBT,DPT,GUESS)                                       SLVWBT   2
C                   COMPUTES WBT GIVEN PATM (IN. HG) DBT AND DPT (DEG F)        SLVWBT   3
      DATA DELTA, TOLP, TOLW /.01,.01,.04/                                      SLVWBT   4
      ERR(W) = PPWVMS(DPT) - PPWVMS(W) + P1*(DBT-W)/(1. + (W-32.)/1571.)        SLVWBT   5
      P1     = .000367 * PATM                                                   SLVWBT   6
      SLVWBT = GUESS                                                            SLVWBT   7
  100 Y1     = ERR(SLVWBT)                                                      SLVWBT   8
      Y2     = ERR(SLVWBT + DELTA)                                              SLVWBT   9
      CRCTN  = DELTA *Y1 /(Y1-Y2)                                               SLVWBT  10
      SLVWBT = SLVWBT + CRCTN                                                   SLVWBT  11
      IF ((ABS(Y1).GT.TOLP) .OR. (ABS(CRCTN).GT.TOLW)) GO TO 100                SLVWBT  12
      RETURN                                                                    SLVWBT  13
      END                                                                       SLVWBT  14
      SUBROUTINE  SOLAD(DEABC,P1,CC,CCF,SALT,COSZ,HDNCLR,BSCLR,                 SOLAD    2
     1                  HDN,HDHOR,HDFHOR)                                       SOLAD    3
C              THIS ROUTINE FINDS THE DIRECT AND DIFFUSE COMPONENTS             SOLAD    4
C              OF RADIATION ON THE HORIZONTAL                                   SOLAD    5
      DIMENSION  DEABC(5)                                                       SOLAD    6
C                                                                               SOLAD    7
      X      = SIN(SALT)                                                        SOLAD    8
      Y      = .309 - .137*X + .394*X*X                                         SOLAD    9
      XK     = X/(DEABC(5) + X) + (P1 - 1.)/(1. - Y)                            SOLAD   10
      HTHCLR = HDNCLR*COSZ + BSCLR                                              SOLAD   11
      HDHOR  = HTHCLR*XK*(1. - CC/10.)                                          SOLAD   12
      HDN    = HDHOR/COSZ                                                       SOLAD   13
      HDFHOR = HTHCLR*(CCF - XK*(1. - CC/10.))                                  SOLAD   14
      RETURN                                                                    SOLAD   15
      END                                                                       SOLAD   16
      SUBROUTINE STATS                                                          STATS    2
C                                                                               STATS    3
C              THIS PROGRAM READS A DOE-2 PACKED WEATHER FILE AND               STATS    4
C              PRODUCES A STATISTICAL SURVEY OF IT                              STATS    5
C                                                                               STATS    6
      COMMON  /CONST/    DTOR, PIOVR2, PIOVR4, PIOV12, IBLNK                    /CONST/  2
C                                                                               /CONST/  3
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /GRUNT/    ADMNTM,ADMXTM,AVRH(4),AVSKYD,AVTDDM,AVTDHR(24),        WTH2     1
     1                   AVTDM,AVTDNM,AVTWM,AVWSDM,AVWSM,AVWSNM,HDD(4),         /GRUNT/  3
     2                   CDD(4),HDH(4),CDH(4),HRDY,HRNT,                        /GRUNT/  4
     3                   LENGTH,MNB0,MNB32,MNTDM,MXB32,MXTDM,MXU90,             /GRUNT/  5
     4                   AVDDNR,AVDTHR,DMXDNR,DMXTHR,DMNDNR,DMNTHR,             /GRUNT/  6
     5                   HMXDNR,HMXTHR,AVXDNR,AVXTHR,                           /GRUNT/  7
     6                   NT(250),NTW(250),AVSKY                                 WTH2     2
C                                                                               /GRUNT/  9
      COMMON  /LOCALD/   STALAT,STALON,SSTALA,CSTALA,TSTALA,HRSLON              /LOCALD/ 2
C                                                                               /LOCALD/ 3
      COMMON  /MONTHC/   BEFORE(12),MDAYS(12),MNAMES(12)                        /MONTHC/ 2
      INTEGER            BEFORE                                                 /MONTHC/ 3
C                                                                               /MONTHC/ 4
      COMMON /MYDATA/AVDST(4),DMXST(4),HMXST(4),DST(4),XST(4)                   /MYDATA/ 2
      COMMON  /PARAMS/   STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 2
      LOGICAL            STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 3
C                                                                               /PARAMS/ 4
      COMMON  /SKRACH/   SKRACH(5000)                                           /SKRACH/ 2
C              USED BY EDIT, MONTHS, PACKER, STATS                              /SKRACH/ 3
C              ONLY EDIT USES ALL OF SKRACH                                     /SKRACH/ 4
C                                                                               /SKRACH/ 5
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON /TIM/ IDOY                                                         /TIME/   2
      COMMON  /WEATHD/   IWDID(5),FIRSTW,LRECX,IWSOL,WLONG,WLAT,IWTZN,          /WEATHD/ 2
     1                   CLRNES,TGRND,IWYR,WBT,DBT,PATM,CLDAMT,ISNOW,           /WEATHD/ 3
     2                   IRAIN,IWNDDR,HUMRAT,DENSTY,ENTHAL,SOLRAD,              /WEATHD/ 4
     3                   DIRSOL,ICLDTY,WNDSPD,IRFCP,DPT                         /WEATHD/ 5
      LOGICAL FIRSTW                                                            /WEATHD/ 6
      DIMENSION          CALC(16)                                               /WEATHD/ 7
      EQUIVALENCE        (WBT,CALC(1))                                          /WEATHD/ 8
C                                                                               /WEATHD/ 9
C                                                                               STATS   19
      DIMENSION  AA(3)                                                          STATS   20
      DIMENSION LOOK(14)                                                        STATS   21
      DIMENSION RAR(4)                                                          STATS   22
      DIMENSION DAT(13,77)                                                      WTH2    56
      DIMENSION  IDATA(13,10)                                                   STATS   24
      DIMENSION  NSMWHS(250)                                                    STATS   25
      DIMENSION  NSUMHS(250)                                                    STATS   26
      DIMENSION  NWINHS(250)                                                    STATS   27
      DIMENSION  TGMNTH(12), CLMNTH(12)                                         STATS   29
C                                                                               STATS   30
      EQUIVALENCE (DAT(1,1),SKRACH(1)),(IDATA(1,1),SKRACH(1002)),               WTH2    57
     1            (NSMWHS(1),SKRACH(1132)),(NSUMHS(1),SKRACH(1382)),            WTH2    58
     2            (NWINHS(1),SKRACH(1632))                                      WTH2    59
C                                                                               STATS   34
      DATA RAR/1HN,1HE,1HS,1HW/                                                 STATS   35
C                                                                               STATS   36
      DATA  AA       /3H1.0,3H2.5,3H5.0/                                        STATS   37
C                                                                               STATS   38
 6000 FORMAT(1H ,14H--- ERROR --- ,33HEND OF FILE READ ON WEATHER FILE.,        STATS   39
     1       24H  PROCESSING TERMINATED. )                                      STATS   40
 7000 FORMAT(6H1YEAR ,I4,2X,5A4,20X,28HMONTHLY WEATHER DATA SUMMARY ,           WTHe     5
     1       41X,'DOE-2.2',A4)                                                  VER22    3
 7001 FORMAT(2H1 ,5A4,2X,                                                       WTHe     7
     1       28HMONTHLY WEATHER DATA SUMMARY ,47X,'DOE-2.2',A4)                 VER22    4
 7010 FORMAT(1H0/20X,11HLATITUDE = ,F6.2,20X,12HLONGITUDE = ,F7.2,              STATS   45
     1       20X,12HTIME ZONE = ,I3)                                            STATS   46
 7020 FORMAT(1H0 /)                                                             STATS   47
 7030 FORMAT(1H0,34X,43H    JAN    FEB    MAR    APR    MAY    JUN   ,          STATS   48
     1       48H   JUL    AUG    SEP    OCT    NOV    DEC   YEAR )              STATS   49
 7040 FORMAT(35H0AVG. TEMP. (F) (DRYBULB)           ,13(1X,F6.1))               STATS   50
 7050 FORMAT(35H AVG. TEMP. (F) (WETBULB)           ,13(1X,F6.1))               STATS   51
 7060 FORMAT(35H0AVG. DAILY MAX. TEMP.              ,13(1X,F6.1))               STATS   52
 7070 FORMAT(35H AVG. DAILY MIN. TEMP.              ,13(1X,F6.1))               STATS   53
 7080 FORMAT(19H0HEATING DEG. DAYS ,9H(BASE 65),7X,13(F7.1)/                    WTHe     9
     1       19X,9H(BASE 60),7X,13(F7.1)/19X,9H(BASE 55),7X,                    WTHe    10
     2       13(F7.1)/19X,9H(BASE 50),7X,13(F7.1))                              WTHe    11
 7110 FORMAT(19H0COOLING DEG. DAYS ,9H(BASE 80),7X,13(1X,F6.1)/                 STATS   57
     1       19X,9H(BASE 75),7X,13(1X,F6.1)/19X,9H(BASE 70),7X,                 STATS   58
     2       13(1X,F6.1)/19X,9H(BASE 65),7X,13(1X,F6.1))                        STATS   59
 7090 FORMAT(22H0HEATING DEG. HRS./24 ,9H(BASE 65),4X,13(F7.1)/                 WTHe    12
     1       22X,9H(BASE 60),4X,13(F7.1)/22X,9H(BASE 55),4X,                    WTHe    13
     2       13(F7.1)/22X,9H(BASE 50),4X,13(F7.1))                              WTHe    14
 7100 FORMAT(22H0COOLING DEG. HRS./24 ,9H(BASE 80),4X,13(1X,F6.1)/              STATS   63
     1       22X,9H(BASE 75),4X,13(1X,F6.1)/22X,9H(BASE 70),4X,                 STATS   64
     2       13(1X,F6.1)/22X,9H(BASE 65),4X,13(1X,F6.1))                        STATS   65
 7140 FORMAT(35H0MAXIMUM TEMP.                      ,13(4X,I3))                 STATS   66
 7150 FORMAT(35H MINIMUM TEMP.                      ,13(4X,I3))                 STATS   67
 7160 FORMAT(35H0NO. DAYS MAX. 90 AND ABOVE         ,13(4X,I3))                 STATS   68
 7170 FORMAT(35H NO. DAYS MAX. 32 AND BELOW         ,13(4X,I3))                 STATS   69
 7180 FORMAT(35H0NO. DAYS MIN. 32 AND BELOW         ,13(4X,I3))                 STATS   70
 7190 FORMAT(35H NO. DAYS MIN.  0 AND BELOW         ,13(4X,I3))                 STATS   71
 7200 FORMAT(35H0AVG. WIND SPEED (MPH)              ,13(1X,F6.1))               STATS   72
 7210 FORMAT(35H0AVG. WIND SPEED (DAY)              ,13(1X,F6.1))               STATS   73
 7220 FORMAT(35H AVG. WIND SPEED (NIGHT)            ,13(1X,F6.1))               STATS   74
 7230 FORMAT(35H0AVG. TEMP. (DAY)                   ,13(1X,F6.1))               STATS   75
 7240 FORMAT(35H AVG. TEMP. (NIGHT)                 ,13(1X,F6.1))               STATS   76
 7245 FORMAT(35H0AVG. SKY COVER                     ,13(1X,F6.1))               WTH2    60
 7250 FORMAT(35H0AVG. SKY COVER (DAY)               ,13(1X,F6.1))               STATS   77
 7260 FORMAT(35H0AVG. REL. HUM. AT  4AM             ,13(1X,F6.1))               STATS   78
 7270 FORMAT(35H                   10AM             ,13(1X,F6.1))               STATS   79
 7280 FORMAT(35H                    4PM             ,13(1X,F6.1))               STATS   80
 7290 FORMAT(35H                   10PM             ,13(1X,F6.1))               STATS   81
 7300 FORMAT(                                                                   STATS   82
     1///54H DESIGN TEMPERATURES ---------------------------------,             STATS   83
     2 25H SUMMER ---------- WINTER //35X,                                      STATS   84
     3 30HPER CENT        T(DRY)  T(WET)                                        STATS   85
     4,14H        T(DRY) )                                                      STATS   86
 7310 FORMAT(38X,A3,13X,I3,5X,I3,11X,I3)                                        STATS   87
 7320 FORMAT(2( 1H0 /),                                                         STATS   88
     146H MONTHLY AVERAGE TEMPERATURES AS A FUNCTION OF                         STATS   89
     2            , 16H HOUR OF THE DAY )                                       STATS   90
 7330 FORMAT( 1H0,21X,43H    JAN    FEB    MAR    APR    MAY    JUN             STATS   91
     1      ,48H   JUL    AUG    SEP    OCT    NOV    DEC   YEAR )              STATS   92
 7340 FORMAT(9X,4HHOUR,9X,13(3X,4H---- ))                                       STATS   93
 7350 FORMAT(10X,I2,10X,13(1X,F6.1))                                            STATS   94
 7360 FORMAT(35H0AVG. DAILY DIRECT NORMAL  SOLAR    ,13(1X,F6.1))               STATS   95
 7370 FORMAT(35H AVG. DAILY TOTAL HORIZNTL SOLAR    ,13(1X,F6.1))               STATS   96
 7380 FORMAT(35H0MAX. DAILY DIRECT NORMAL  SOLAR    ,13(1X,F6.1))               STATS   97
 7390 FORMAT(35H MAX. DAILY TOTAL HORIZNTL SOLAR    ,13(1X,F6.1))               STATS   98
 7400 FORMAT(35H MIN. DAILY DIRECT NORMAL  SOLAR    ,13(1X,F6.1))               STATS   99
 7410 FORMAT(35H MIN. DAILY TOTAL HORIZNTL SOLAR    ,13(1X,F6.1))               STATS  100
 7420 FORMAT(35H0MAX. HRLY  DIRECT NORMAL  SOLAR    ,13(1X,F6.1))               STATS  101
 7430 FORMAT(35H MAX. HRLY  TOTAL HORIZNTL SOLAR    ,13(1X,F6.1))               STATS  102
 7440 FORMAT(35H AVG. MAX. HRLY DIRECT NORML SOLAR  ,13(1X,F6.1))               STATS  103
 7450 FORMAT(35H AVG. MAX. HRLY TOTAL HRZNTL SOLAR  ,13(1X,F6.1))               STATS  104
 7460 FORMAT(////1H ,19HGROUND TEMPERATURES,2X,12(1X,F6.1))                     STATS  105
 7470 FORMAT(1H ,17HCLEARNESS NUMBERS,4X,12(1X,F6.2))                           STATS  106
 9360 FORMAT(35H0AVG. DAILY TOTAL VERTICAL SOLAR    )                           STATS  107
 9380 FORMAT(35H0MAX. DAILY TOTAL VERTICAL SOLAR    )                           STATS  108
 9420 FORMAT(35H0MAX. HRLY  TOTAL VERTICAL SOLAR    )                           STATS  109
 9200 FORMAT(1H ,16X,7HAZIMUTH)                                                 STATS  110
 9210 FORMAT(1H ,19X,A1,14X,13(1X,F6.1))                                        STATS  111
C                                                                               STATS  112
C              INITIALIZE                                                       STATS  113
      IF (.NOT. NEWPAK)  REWIND  INWTH                                          STATS  114
C              READ WEATHER TAPE TO DEFINE LATITUDE AND LONGITUDE               STATS  115
      READ(INWTH,END=3010) LOOK                                                 STATS  116
      IFX    = LOOK(14)                                                         STATS  117
      IF (LOOK(11) .EQ. 0) IFX=0                                                STATS  118
      IFX    = IFX+1                                                            STATS  119
      BACKSPACE INWTH                                                           STATS  120
      IF (IFX .GE. 3) GO TO 7                                                   STATS  121
      READ(INWTH,END=3010) IWDID(1),IWDID(2),IWYR,WLAT,WLONG,IWTZN,             STATS  122
     1                     DUM,DUM,DUM,DUM                                      STATS  123
      GO TO 9                                                                   STATS  124
    7 CONTINUE                                                                  STATS  125
      READ (INWTH,END=3010) DUM                                                 STATS  126
      READ (INWTH,END=3010) IWDID,IWYR,WLAT,WLONG,IWTZN,DUM,DUM,                STATS  127
     1                      DUM,DUM                                             STATS  128
    9 CONTINUE                                                                  STATS  129
      REWIND INWTH                                                              STATS  130
      IDOY   = 0                                                                STATS  131
      STALAT = WLAT*DTOR                                                        STATS  132
      STALON = WLONG*DTOR                                                       STATS  133
      SSTALA = SIN(STALAT)                                                      STATS  134
      CSTALA = COS(STALAT)                                                      STATS  135
      TSTALA = SSTALA/CSTALA                                                    STATS  136
      HRDYY  = 0.0                                                              STATS  137
      HRNTY  = 0.0                                                              STATS  138
      NHYR   = 0                                                                STATS  139
      NDYR   = 0                                                                STATS  140
      DO 10 J=1,77                                                              WTH2    61
      DO 10 I=1,13                                                              STATS  142
   10 DAT(I,J) = 0.0                                                            STATS  143
      DO 20 J=1,10                                                              STATS  144
      DO 20 I=1,13                                                              STATS  145
   20 IDATA(I,J) = 0                                                            STATS  146
      DO 30 I=1,250                                                             STATS  147
      NSMWHS(I) = 0                                                             STATS  148
      NSUMHS(I) = 0                                                             STATS  149
   30 NWINHS(I) = 0                                                             STATS  150
      DO 1040 I = 1,12                                                          STATS  151
      IMNTH  = I                                                                STATS  152
      CALL  MONTHS(NDYR)                                                        STATS  153
      IF (IEOF) GO TO 3010                                                      STATS  154
      TGMNTH(IMNTH) = TGRND                                                     STATS  155
      CLMNTH(IMNTH) = CLRNES                                                    STATS  156
      DAT(I,1) = AVWSM                                                          STATS  157
      IF (IWSOL .GT. 1)  DAT(I,1) = DAT(I,1)*1.1507794                          STATS  158
      DAT(I,2) = AVTDM                                                          STATS  159
      DAT(I,3) = ADMXTM                                                         STATS  160
      DAT(I,4) = ADMNTM                                                         STATS  161
      DAT(I,5) = HDD(1)                                                         STATS  162
      DAT(I,6) = HDD(2)                                                         STATS  163
      DAT(I,7) = AVWSDM                                                         STATS  164
      DAT(I,8) = AVWSNM                                                         STATS  165
      IF (IWSOL .GT. 1)  DAT(I,7) = DAT(I,7)*1.1507794                          STATS  166
      IF (IWSOL .GT. 1)  DAT(I,8) = DAT(I,8)*1.1507794                          STATS  167
      DAT(I,9) = AVTDDM                                                         STATS  168
      DAT(I,10) = AVTDNM                                                        STATS  169
      DAT(I,11) = AVSKYD                                                        WTH2    62
      DAT(I,77) = AVSKY                                                         WTH2    63
      DAT(I,40) = AVTWM                                                         STATS  171
      DAT(I,41) = HDD(3)                                                        STATS  172
      DAT(I,42) = HDD(4)                                                        STATS  173
      DAT(I,43) = CDD(1)                                                        STATS  174
      DAT(I,44) = CDD(2)                                                        STATS  175
      DAT(I,67) = CDD(3)                                                        STATS  176
      DAT(I,68) = CDD(4)                                                        STATS  177
      DO 100 J=1,4                                                              STATS  178
      DAT(I,68+J) = HDH(J)                                                      STATS  179
      DAT(I,72+J) = CDH(J)                                                      STATS  180
  100 CONTINUE                                                                  STATS  181
      DO 1000 J = 1,4                                                           STATS  182
      DAT(13,11+J) = DAT(13,11+J) + FLOAT(LENGTH)*AVRH(J)                       STATS  183
      DAT(I,11+J) = AVRH(J)                                                     STATS  184
 1000 CONTINUE                                                                  STATS  185
      DO 1010 J = 1,24                                                          STATS  186
      DAT(13,15+J) = DAT(13,15+J) + FLOAT(LENGTH)*AVTDHR(J)                     STATS  187
      DAT(I,15+J) = AVTDHR(J)                                                   STATS  188
 1010 CONTINUE                                                                  STATS  189
      DAT(I,45) = AVDDNR                                                        STATS  190
      DAT(I,46) = AVDTHR                                                        STATS  191
      DAT(I,47) = DMXDNR                                                        STATS  192
      DAT(I,48) = DMXTHR                                                        STATS  193
      DAT(I,49) = DMNDNR                                                        STATS  194
      DAT(I,50) = DMNTHR                                                        STATS  195
      DAT(I,51) = HMXDNR                                                        STATS  196
      DAT(I,52) = HMXTHR                                                        STATS  197
      DAT(I,53) = AVXDNR                                                        STATS  198
      DAT(I,54) = AVXTHR                                                        STATS  199
      DO 910 IK=1,4                                                             STATS  200
      DAT(I,IK+54)=AVDST(IK)                                                    STATS  201
      DAT(I,IK+58)=DMXST(IK)                                                    STATS  202
      DAT(I,IK+62)=HMXST(IK)                                                    STATS  203
  910 CONTINUE                                                                  STATS  204
 1014 CONTINUE                                                                  STATS  205
      IDATA(I,1) = MXTDM                                                        STATS  206
      IDATA(I,2) = MNTDM                                                        STATS  207
      IDATA(I,3) = MXU90                                                        STATS  208
      IDATA(I,4) = MXB32                                                        STATS  209
      IDATA(I,5) = MNB32                                                        STATS  210
      IDATA(I,6) = MNB0                                                         STATS  211
      NHYR   = NHYR + LENGTH*24                                                 STATS  212
      NDYR   = NDYR + LENGTH                                                    STATS  213
      HRNTY  = HRNTY + HRNT                                                     STATS  214
      HRDYY  = HRDYY + HRDY                                                     STATS  215
      DAT(13,1) = DAT(13,1) + FLOAT(LENGTH)*AVWSM                               STATS  216
      DAT(13,2) = DAT(13,2) + FLOAT(LENGTH)*AVTDM                               STATS  217
      DAT(13,3) = DAT(13,3) + FLOAT(LENGTH)*ADMXTM                              STATS  218
      DAT(13,4) = DAT(13,4) + FLOAT(LENGTH)*ADMNTM                              STATS  219
      DAT(13,5) = DAT(13,5) + HDD(1)                                            STATS  220
      DAT(13,6) = DAT(13,6) + HDD(2)                                            STATS  221
      DAT(13,7) = DAT(13,7) + HRDY*AVWSDM                                       STATS  222
      DAT(13,8) = DAT(13,8) + HRNT*AVWSNM                                       STATS  223
      DAT(13,9) = DAT(13,9) + HRDY*AVTDDM                                       STATS  224
      DAT(13,10) = DAT(13,10) + HRNT*AVTDNM                                     STATS  225
      DAT(13,11) = DAT(13,11) + HRDY*AVSKYD                                     WTH2    64
      DAT(13,77) = DAT(13,77) + FLOAT(LENGTH)*AVSKY                             WTH2    65
      IDATA(13,3) = IDATA(13,3) + MXU90                                         STATS  227
      IDATA(13,4) = IDATA(13,4) + MXB32                                         STATS  228
      IDATA(13,5) = IDATA(13,5) + MNB32                                         STATS  229
      IDATA(13,6) = IDATA(13,6) + MNB0                                          STATS  230
      DAT(13,40) = DAT(13,40) + FLOAT(LENGTH)*AVTWM                             STATS  231
      DAT(13,41) = DAT(13,41) + HDD(3)                                          STATS  232
      DAT(13,42) = DAT(13,42) + HDD(4)                                          STATS  233
      DAT(13,43) = DAT(13,43) + CDD(1)                                          STATS  234
      DAT(13,44) = DAT(13,44) + CDD(2)                                          STATS  235
      DAT(13,67) = DAT(13,67) + CDD(3)                                          STATS  236
      DAT(13,68) = DAT(13,68) + CDD(4)                                          STATS  237
      DO 915 J=1,4                                                              STATS  238
      DAT(13,68+J) = DAT(13,68+J) + HDH(J)                                      STATS  239
      DAT(13,72+J) = DAT(13,72+J) + CDH(J)                                      STATS  240
  915 CONTINUE                                                                  STATS  241
      DAT(13,45) = DAT(13,45) + FLOAT(LENGTH) * AVDDNR                          STATS  242
      DAT(13,46) = DAT(13,46) + FLOAT(LENGTH) * AVDTHR                          STATS  243
      DAT(13,53) = DAT(13,53) + FLOAT(LENGTH) * AVXDNR                          STATS  244
      DAT(13,54) = DAT(13,54) + FLOAT(LENGTH) * AVXTHR                          STATS  245
      DO 920 IK=1,4                                                             STATS  246
      DAT(13,IK+54)=DAT(13,IK+54)+FLOAT(LENGTH)*AVDST(IK)                       STATS  247
  920 CONTINUE                                                                  STATS  248
 1015 CONTINUE                                                                  STATS  249
      IF (WLAT .LT. 0.)  GO TO 3020                                             STATS  250
      GO TO (3000,3000,1040,1040,1040,3050,                                     STATS  251
     1       3050,3050,3050,1040,1040,3000),I                                   STATS  252
 3020 GO TO (3050,3050,3050,1040,1040,3000,                                     STATS  253
     1       3000,3000,1040,1040,1040,3050),I                                   STATS  254
 3050 DO 1020 J=1,250                                                           STATS  255
      NSMWHS(J) = NSMWHS(J) + NTW(J)                                            STATS  256
      NSUMHS(J) = NSUMHS(J) + NT(J)                                             STATS  257
 1020 CONTINUE                                                                  STATS  258
      GOTO 1040                                                                 STATS  259
 3000 CONTINUE                                                                  STATS  260
      DO 1030 J = 1,250                                                         STATS  261
      NWINHS(J) = NWINHS(J) + NT(J)                                             STATS  262
 1030 CONTINUE                                                                  STATS  263
 1040 CONTINUE                                                                  STATS  264
      M      = MAXVEC(IDATA(1,1),12)                                            STATS  265
      N      = MINVEC(IDATA(1,2),12)                                            STATS  266
      IDATA(13,1) = M                                                           STATS  267
      IDATA(13,2) = N                                                           STATS  268
      DO 1045 J = 1,2                                                           STATS  269
      I      = 44 + J                                                           STATS  270
      DAT(13,I) = DAT(13,I)/FLOAT(NDYR)                                         STATS  271
      I      = 46 + J                                                           STATS  272
      DAT(13,I) = AMAX(DAT(1,I),12)                                             STATS  273
      I      = 48 + J                                                           STATS  274
      DAT(13,I) = AMIN(DAT(1,I),12)                                             STATS  275
      I      = 50 + J                                                           STATS  276
      DAT(13,I) = AMAX(DAT(1,I),12)                                             STATS  277
      I      = 52 + J                                                           STATS  278
      DAT(13,I) = DAT(13,I)/FLOAT(NDYR)                                         STATS  279
 1045 CONTINUE                                                                  STATS  280
      DO 930 IK=1,4                                                             STATS  281
      DAT(13,IK+54)=DAT(13,IK+54)/FLOAT(NDYR)                                   STATS  282
      DAT(13,IK+58)=AMAX(DAT(1,IK+58),12)                                       STATS  283
      DAT(13,IK+62)=AMAX(DAT(1,IK+62),12)                                       STATS  284
  930 CONTINUE                                                                  STATS  285
 1046 CONTINUE                                                                  STATS  286
      DAT(13,1) = 1.1507794*DAT(13,1)/FLOAT(NDYR)                               STATS  287
      DAT(13,2) = DAT(13,2)/FLOAT(NDYR)                                         STATS  288
      DAT(13,40) = DAT(13,40)/FLOAT(NDYR)                                       STATS  289
      DAT(13,3) = DAT(13,3)/FLOAT(NDYR)                                         STATS  290
      DAT(13,4) = DAT(13,4)/FLOAT(NDYR)                                         STATS  291
      DAT(13,7) = 1.1507794*DAT(13,7)/HRDYY                                     STATS  292
      DAT(13,8) = 1.1507794*DAT(13,8)/HRNTY                                     STATS  293
      DAT(13,9) = DAT(13,9)/HRDYY                                               STATS  294
      DAT(13,10) = DAT(13,10)/HRNTY                                             STATS  295
      DAT(13,11) = DAT(13,11)/HRDYY                                             STATS  296
      DAT(13,77) = DAT(13,77)/FLOAT(NDYR)                                       WTH2    66
      DO 1050 J = 1,4                                                           STATS  297
      DAT(13,11+J) = DAT(13,11+J)/FLOAT(NDYR)                                   STATS  298
 1050 CONTINUE                                                                  STATS  299
      DO 1060 J = 1,24                                                          STATS  300
      DAT(13,15+J) = DAT(13,15+J)/FLOAT(NDYR)                                   STATS  301
 1060 CONTINUE                                                                  STATS  302
      IF (WLAT .LT. 0.)  GO TO 4000                                             STATS  303
      NHRSUM = 2928                                                             STATS  304
      NHRWIN = 62 + MDAYS(2)                                                    STATS  305
      NHRWIN = NHRWIN*24                                                        STATS  306
      GO TO 4010                                                                STATS  307
 4000 NHRSUM = 93 + MDAYS(2)                                                    STATS  308
      NHRSUM = NHRSUM*24                                                        STATS  309
      NHRWIN = 2208                                                             STATS  310
 4010 CONTINUE                                                                  STATS  311
      CALL SUMDT(NSUMHS,NHRSUM,IT01,IT025,IT05)                                 STATS  312
      CALL SUMDT(NSMWHS,NHRSUM,ITW01,ITW025,ITW05)                              STATS  313
      CALL WINDT(NWINHS,NHRWIN,IT99,IT975)                                      STATS  314
      IF (IWSOL .LT. 4)  WRITE(OUTFIL,7000)IWYR,IWDID,VERS                      STATS  315
      IF (IWSOL .GE. 4)  WRITE(OUTFIL,7001)     IWDID,VERS                      STATS  316
      WRITE(OUTFIL,7010)WLAT,WLONG,IWTZN                                        STATS  317
      WRITE(OUTFIL,7020)                                                        STATS  318
      WRITE(OUTFIL,7030)                                                        STATS  319
      WRITE(OUTFIL,7040)(DAT(I,2),I=1,13)                                       STATS  320
      WRITE(OUTFIL,7050)(DAT(I,40),I=1,13)                                      STATS  321
      WRITE(OUTFIL,7060)(DAT(I,3),I=1,13)                                       STATS  322
      WRITE(OUTFIL,7070)(DAT(I,4),I=1,13)                                       STATS  323
      WRITE (OUTFIL,7080) ((DAT(I,J),I=1,13),J=5,6),                            STATS  324
     1                    ((DAT(I,J),I=1,13),J=41,42)                           STATS  325
      WRITE (OUTFIL,7110) ((DAT(I,J),I=1,13),J=43,44),                          STATS  326
     1                    ((DAT(I,J),I=1,13),J=67,68)                           STATS  327
      WRITE (OUTFIL,7090) ((DAT(I,J),I=1,13),J=69,72)                           STATS  328
      WRITE (OUTFIL,7100) ((DAT(I,J),I=1,13),J=73,76)                           STATS  329
      WRITE(OUTFIL,7140)(IDATA(I,1),I=1,13)                                     STATS  330
      WRITE(OUTFIL,7150)(IDATA(I,2),I=1,13)                                     STATS  331
      WRITE(OUTFIL,7160)(IDATA(I,3),I=1,13)                                     STATS  332
      WRITE(OUTFIL,7170)(IDATA(I,4),I=1,13)                                     STATS  333
      WRITE(OUTFIL,7180)(IDATA(I,5),I=1,13)                                     STATS  334
      WRITE(OUTFIL,7190)(IDATA(I,6),I=1,13)                                     STATS  335
      WRITE(OUTFIL,7200)(DAT(I,1),I=1,13)                                       STATS  336
      WRITE(OUTFIL,7210)(DAT(I,7),I=1,13)                                       STATS  337
      WRITE(OUTFIL,7220)(DAT(I,8),I=1,13)                                       STATS  338
      WRITE(OUTFIL,7230)(DAT(I,9),I=1,13)                                       STATS  339
      WRITE(OUTFIL,7240)(DAT(I,10),I=1,13)                                      STATS  340
      WRITE(OUTFIL,7245)(DAT(I,77),I=1,13)                                      WTH2    67
      WRITE(OUTFIL,7250)(DAT(I,11),I=1,13)                                      STATS  341
      WRITE(OUTFIL,7260)(DAT(I,12),I=1,13)                                      STATS  342
      WRITE(OUTFIL,7270)(DAT(I,13),I=1,13)                                      STATS  343
      WRITE(OUTFIL,7280)(DAT(I,14),I=1,13)                                      STATS  344
      WRITE(OUTFIL,7290)(DAT(I,15),I=1,13)                                      STATS  345
      WRITE(OUTFIL,7001)IWDID,VERS                                              STATS  346
      WRITE(OUTFIL,7010)WLAT,WLONG,IWTZN                                        STATS  347
      WRITE(OUTFIL,7020)                                                        STATS  348
      WRITE(OUTFIL,7030)                                                        STATS  349
      WRITE(OUTFIL,7360)(DAT(I,45),I=1,13)                                      STATS  350
      WRITE(OUTFIL,7370)(DAT(I,46),I=1,13)                                      STATS  351
      WRITE(OUTFIL,7380)(DAT(I,47),I=1,13)                                      STATS  352
      WRITE(OUTFIL,7390)(DAT(I,48),I=1,13)                                      STATS  353
      WRITE(OUTFIL,7400)(DAT(I,49),I=1,13)                                      STATS  354
      WRITE(OUTFIL,7410)(DAT(I,50),I=1,13)                                      STATS  355
      WRITE(OUTFIL,7420)(DAT(I,51),I=1,13)                                      STATS  356
      WRITE(OUTFIL,7430)(DAT(I,52),I=1,13)                                      STATS  357
      WRITE(OUTFIL,7440)(DAT(I,53),I=1,13)                                      STATS  358
      WRITE(OUTFIL,7450)(DAT(I,54),I=1,13)                                      STATS  359
      WRITE(OUTFIL,7020)                                                        STATS  360
      WRITE(OUTFIL,9360)                                                        STATS  361
      WRITE(OUTFIL,9200)                                                        STATS  362
      DO 940 IK=1,4                                                             STATS  363
      WRITE(OUTFIL,9210)RAR(IK),(DAT(I,IK+54),I=1,13)                           STATS  364
  940 CONTINUE                                                                  STATS  365
      WRITE(OUTFIL,9380)                                                        STATS  366
      WRITE(OUTFIL,9200)                                                        STATS  367
      DO 945 IK=1,4                                                             STATS  368
      WRITE(OUTFIL,9210) RAR(IK),(DAT(I,IK+58),I=1,13)                          STATS  369
  945 CONTINUE                                                                  STATS  370
      WRITE(OUTFIL,9420)                                                        STATS  371
      WRITE(OUTFIL,9200)                                                        STATS  372
      DO 950 IK=1,4                                                             STATS  373
      WRITE(OUTFIL,9210) RAR(IK),(DAT(I,IK+62),I=1,13)                          STATS  374
  950 CONTINUE                                                                  STATS  375
      WRITE(OUTFIL,7001)     IWDID,VERS                                         STATS  376
      GO TO 1066                                                                STATS  377
 1066 WRITE(OUTFIL,7300)                                                        STATS  378
      WRITE(OUTFIL,7310)AA(1),IT01,ITW01,IT99,AA(2),IT025,ITW025,               STATS  379
     1                  IT975,AA(3),                                            STATS  380
     2 IT05,ITW05                                                               STATS  381
      WRITE(OUTFIL,7320)                                                        STATS  382
      WRITE(OUTFIL,7330)                                                        STATS  383
      WRITE(OUTFIL,7340)                                                        STATS  384
      DO 1070 IH = 1,24                                                         STATS  385
      IHR    = IH - 1                                                           STATS  386
      WRITE(OUTFIL,7350)IHR,(DAT(I,IH+15),I=1,13)                               STATS  387
 1070 CONTINUE                                                                  STATS  388
      WRITE(OUTFIL,7460) TGMNTH                                                 STATS  389
      WRITE(OUTFIL,7470) CLMNTH                                                 STATS  390
      WRITE (STOUT) ((DAT(I,J),I=1,13),J=1,66)                                  STATS  391
      RETURN                                                                    STATS  392
 3010 STOPIT = .TRUE.                                                           STATS  393
      WRITE(OUTFIL,6000)                                                        STATS  394
      RETURN                                                                    STATS  395
      END                                                                       STATS  396
      SUBROUTINE SUMDT(NB,NHRSUM,IT01,IT025,IT05)                               SUMDT    2
C                                                                               SUMDT    3
C              THIS SUBROUTINE FINDS THE 1%, 2.5%, AND 5% MARKS IN              SUMDT    4
C              ARRAY 'NB' IN ORDER TO DETERMINE THE PERCENTILE                  SUMDT    5
C              RANKS                                                            SUMDT    6
C                                                                               SUMDT    7
      DIMENSION  NB(250)                                                        SUMDT    8
C                                                                               SUMDT    9
      IT01   = 0                                                                SUMDT   10
      IT025  = 0                                                                SUMDT   11
      IT05   = 0                                                                SUMDT   12
      NSUM   = 0                                                                SUMDT   13
      DO 1000 I = 1,150                                                         SUMDT   14
      IG     = 251 - I                                                          SUMDT   15
      NSUM   = NSUM + NB(IG)                                                    SUMDT   16
      ITEMP  = 150 - I                                                          SUMDT   17
      PERC   = 100.0*FLOAT(NSUM)/FLOAT(NHRSUM)                                  SUMDT   18
      IF(PERC .LT. 1.0) IT01 = ITEMP                                            SUMDT   19
      IF(PERC .LT. 2.5) IT025 = ITEMP                                           SUMDT   20
      IF(PERC .GE. 5.0) GOTO 3000                                               SUMDT   21
      IT05   = ITEMP                                                            SUMDT   22
 1000 CONTINUE                                                                  SUMDT   23
 3000 CONTINUE                                                                  SUMDT   24
      RETURN                                                                    SUMDT   25
      END                                                                       SUMDT   26
      SUBROUTINE SUNCLR(IM,DEABC,CSZ,DNCLR,DFCLR,TOTCLR)                        SUNCLR   2
C                                                                               SUNCLR   3
C              CALCULATES CLEAR SKY TOTAL SOLAR                                 SUNCLR   4
C                                                                               SUNCLR   5
C              INPUTS                                                           SUNCLR   6
C              ------                                                           SUNCLR   7
C                                                                               SUNCLR   8
C              IM       - MONTH                                                 SUNCLR   9
C              DEABC(3) - CONSTANT USED IN ASHRAE CLEAR SKY FORMULA             SUNCLR  10
C              DEABC(4) - CONSTANT USED IN ASHRAE CLEAR SKY FORMULA             SUNCLR  11
C              DEABC(5) - CONSTANT USED IN ASHRAE CLEAR SKY FORMULA             SUNCLR  12
C              CSZ      - COSINE OF THE SOLAR ZENITH ANGLE                      SUNCLR  13
C                                                                               SUNCLR  14
C              OUTPUTS                                                          SUNCLR  15
C              -------                                                          SUNCLR  16
C                                                                               SUNCLR  17
C              DNCLR    - CLEAR SKY DIRECT NORMAL SOLAR RAD (BTU/HR-SQFT)       SUNCLR  18
C              DFCLR    - CLEAR SKY DIFFUSE (BTU/HR-SQFT)                       SUNCLR  19
C              TOTCLR   - CLEAR SKY TOTAL HORIZONTAL (BTU/HR-SQFT)              SUNCLR  20
C                                                                               SUNCLR  21
C              ALSO USED                                                        SUNCLR  22
C              ---------                                                        SUNCLR  23
C                                                                               SUNCLR  24
C              CN(1-12) - MONTHLY CLEARNESS NUMBERS                             SUNCLR  25
C                                                                               SUNCLR  26
      INTEGER IM                                                                SUNCLR  27
      REAL DEABC(5),CSZ,DNCLR,DFCLR,TOTCLR                                      SUNCLR  28
C                                                                               SUNCLR  29
      COMMON  /PCKINT/   KYR,KSTAT,CN(12),TG(12),KTIM,KARD(500),KYRL            -089     1
C                                                                               /PCKINT/ 3
C                                                                               SUNCLR  31
      DNCLR = 0.                                                                SUNCLR  32
      DFCLR = 0.                                                                SUNCLR  33
      TOTCLR = 0.                                                               SUNCLR  34
      IF (CSZ.LT.0.005) GO TO 1000                                              SUNCLR  35
      DNCLR = DEABC(3)*CN(IM)*EXP(-DEABC(4)/CSZ)                                SUNCLR  36
      DFCLR = DEABC(5)*DNCLR/(CN(IM)*CN(IM))                                    SUNCLR  37
      TOTCLR = DNCLR*CSZ + DFCLR                                                SUNCLR  38
 1000 CONTINUE                                                                  SUNCLR  39
      RETURN                                                                    SUNCLR  40
      END                                                                       SUNCLR  41
      SUBROUTINE SUNCOS(IH,DEABC,COSZIN,COSZAV,SALT,FSUN)                       SUNCOS   2
C                                                                               SUNCOS   3
C              CALCULATES THE COSINE OF THE SOLAR ZENITH ANGLE                  SUNCOS   4
C                                                                               SUNCOS   5
C                                                                               SUNCOS   6
C              INPUTS                                                           SUNCOS   7
C              ------                                                           SUNCOS   8
C                                                                               SUNCOS   9
C              IH       - HOUR OF THE DAY (1-24)                                SUNCOS  10
C              DEABC(1) - SOLAR DECLINATION ANGLE (RADIANS)                     SUNCOS  11
C              DEABC(2) - EQUATION OF TIME (HOURS)                              SUNCOS  12
C                                                                               SUNCOS  13
C              OUTPUTS                                                          SUNCOS  14
C              -------                                                          SUNCOS  15
C                                                                               SUNCOS  16
C              COSZIN   - COS OF ZENITH ANGLE INTEGRATED OVER TIME BIN          SUNCOS  17
C              COSZAV   - AVERAGE COS OF ZENITH ANGLE FOR TIME BIN              SUNCOS  18
C              SALT     - SOLAR ALTITUDE IN RADIANS                             SUNCOS  19
C              FSUN     - FRACTION OF HOUR SUN IS UP                            SUNCOS  20
C                                                                               SUNCOS  21
C              ALSO USED                                                        SUNCOS  22
C              ---------                                                        SUNCOS  23
C                                                                               SUNCOS  24
C              DTOR     - DEGREES TO RADIANS = PI/180                           SUNCOS  25
C              NTZ      - TIME ZONE (0=GREENWICH; 8=PST)                        SUNCOS  26
C              PIOV12   - PI/12                                                 SUNCOS  27
C              STALAT   - LATITUDE IN RADIANS                                   SUNCOS  28
C              XLONG    - LONGITUDE IN DEGREES                                  SUNCOS  29
C                                                                               SUNCOS  30
      INTEGER IH                                                                SUNCOS  31
      REAL DEABC(5),COSZIN,COSZAV,SALT                                          SUNCOS  32
C                                                                               SUNCOS  33
      COMMON  /CONST/    DTOR, PIOVR2, PIOVR4, PIOV12, IBLNK                    /CONST/  2
C                                                                               /CONST/  3
      COMMON  /LOCALD/   STALAT,STALON,SSTALA,CSTALA,TSTALA,HRSLON              /LOCALD/ 2
C                                                                               /LOCALD/ 3
      COMMON  /PACKEC/   NTZ,ISTAT,XLAT,XLONG,                                  /PACKEC/ 2
     1                   IYR,INTINT,DDBT,IBEGH,NBS,IDOYSH(5),ISSHFT(5),         /PACKEC/ 3
     2                   ISHFT,ALT,SKYCOV(12),ILOC                              073a     1
C                                                                               /PACKEC/ 5
C                                                                               SUNCOS  37
      COSZIN = 0.                                                               SUNCOS  38
      COSZAV = 0.                                                               SUNCOS  39
      SALT = 0.                                                                 SUNCOS  40
      FSUN = 0.                                                                 SUNCOS  41
C              GET HOUR ANGLE AT UPPER AND LOWER HOUR BIN EDGE                  SUNCOS  42
C              THIS IS LOCAL TIME!                                              SUNCOS  43
      UL     = FLOAT(IH) - 12. + FLOAT(NTZ) + DEABC(2) - XLONG/15.              SUNCOS  44
      BL     = UL - 1.                                                          SUNCOS  45
C              SUNRISE AND SUNSET HOUR ANGLES                                   SUNCOS  46
      SSHA   = ACOS(-TAN(STALAT)*TAN(DEABC(1)))/PIOV12                          SUNCOS  47
      SRHA   = -SSHA                                                            SUNCOS  48
      IF ((UL .LE. SRHA) .OR. (BL .GE. SSHA))  GO TO 1000                       SUNCOS  49
      IF (SRHA .GT. BL)  BL = SRHA                                              SUNCOS  50
      IF (SSHA .LT. UL)  UL = SSHA                                              SUNCOS  51
      IF ((UL-BL) .LT. 0.02)  GO TO 1000                                        SUNCOS  52
      FSUN = UL - BL                                                            SUNCOS  53
      A      = SIN(DEABC(1))*SIN(STALAT)                                        SUNCOS  54
      B      = COS(DEABC(1))*COS(STALAT)                                        SUNCOS  55
C              INTEGRATE SOLAR Z DIREC. COSINE OVER BIN                         SUNCOS  56
      COSZIN = A*(UL-BL) + B*(SIN(PIOV12*UL)-SIN(PIOV12*BL))/PIOV12             SUNCOS  57
C              AVERAGE COSINE OF THE SOLAR ZENITH ANGLE FOR THE HOUR            SUNCOS  58
      COSZAV = COSZIN/(UL-BL)                                                   SUNCOS  59
      SALT = ASIN(COSZAV)                                                       SUNCOS  60
 1000 CONTINUE                                                                  SUNCOS  61
      RETURN                                                                    SUNCOS  62
      END                                                                       SUNCOS  63
      SUBROUTINE SUNPRM(IDAY,DEABC)                                             SUNPRM   2
C                                                                               SUNPRM   3
C              THIS SUBROUTINE RETURNS FIVE SUN PARAMETERS IN                   SUNPRM   4
C              ARRAY 'DEABC'                                                    SUNPRM   5
C                                                                               SUNPRM   6
      DIMENSION  DEABC(5)                                                       SUNPRM   7
      COMMON  /LOCALD/   STALAT,STALON,SSTALA,CSTALA,TSTALA,HRSLON              /LOCALD/ 2
C                                                                               /LOCALD/ 3
C                                                                               SUNPRM   8
      C1     = COS(0.01721*FLOAT(IDAY))                                         SUNPRM   9
      S1     = SIN(0.01721*FLOAT(IDAY))                                         SUNPRM  10
      S2     = 2.0*S1*C1                                                        SUNPRM  11
      C2     = C1*C1 - S1*S1                                                    SUNPRM  12
      C3     = C1*C2 - S1*S2                                                    SUNPRM  13
      S3     = C1*S2 + S1*C2                                                    SUNPRM  14
      DEABC(1) = .00527 - .4001*C1 - .003996*C2 - .00424*C3 + .0672*S1          SUNPRM  15
      DEABC(2) = .696E-4+.706E-2*C1-.0533*C2-.157E-2*C3-.122*S1-.156*S2         SUNPRM  17
     1  - 0.556E-2*S3                                                           SUNPRM  18
      DEABC(3) = 368.44+24.52*C1-1.14*C2-1.09*C3+0.58*S1-0.18*S2+0.28*S3        SUNPRM  19
C                                                                               SUNPRM  20
C              SHIFT THE EXINCTION AND DIFFUSE COEFFICIENT CURVES               SUNPRM  21
C              BY 6 MONTHS FOR THE SOUTHERN HEMISPHERE                          SUNPRM  22
C                                                                               SUNPRM  23
      IF (STALAT.GE.0.) GO TO 100                                               -073     2
      S1 = -S1                                                                  SUNPRM  25
      C1 = -C1                                                                  SUNPRM  26
      S3 = -S3                                                                  SUNPRM  27
      C3 = -C3                                                                  SUNPRM  28
  100 CONTINUE                                                                  SUNPRM  29
      DEABC(4) = .1717-.0344*C1+.0032*C2+.0024*C3-.0043*S1-.0008*S3             SUNPRM  30
      DEABC(5) = .0905-.0410*C1+.0073*C2+.0015*C3-.0034*S1+.0004*S2             SUNPRM  31
     1  - 0.0006*S3                                                             SUNPRM  32
      RETURN                                                                    SUNPRM  33
      END                                                                       SUNPRM  34
      SUBROUTINE SUNSPL(IDOY,COSZIN,COSZAV,SOLHOR,DIRH,DIRN,DIF)                SUNSPL   2
C                                                                               SUNSPL   3
C              CALCULATES THE BEAM AND DIFFUSE SOLAR RADIATION GIVEN            SUNSPL   4
C              THE TOTAL HORIZONTAL SOLAR RADIATION                             SUNSPL   5
C                                                                               SUNSPL   6
C              INPUTS                                                           SUNSPL   7
C              ------                                                           SUNSPL   8
C                                                                               SUNSPL   9
C              IDOY     - DAY OF THE YEAR (1-365)                               SUNSPL  10
C              COSZIN   - COS OF ZENITH ANGLE INTEGRATED OVER TIME BIN          SUNSPL  11
C              COSZAV   - AVERAGE COS OF ZENITH ANGLE FOR TIME BIN              SUNSPL  12
C              SOLHOR   - TOTAL SOLAR HORIZONTAL (BTU/HR-SQFT)                  SUNSPL  13
C                                                                               SUNSPL  14
C              OUTPUTS                                                          SUNSPL  15
C              -------                                                          SUNSPL  16
C                                                                               SUNSPL  17
C              DIRH     - BEAM RAD ON A HORIZ SURFACE (BTU/HR-SQFT)             SUNSPL  18
C              DIRN     - BEAM RAD ON A SURF NORMAL TO BEAM (BTU/HR-SQFT)       SUNSPL  19
C              DIF      - DIFFUSE SOLAR RAD (BTU/HR-SQFT)                       SUNSPL  20
C                                                                               SUNSPL  21
C              ALSO USED                                                        SUNSPL  22
C              ---------                                                        SUNSPL  23
C                                                                               SUNSPL  24
C              DTOR     - DEGREES TO RADIANS = PI/180                           SUNSPL  25
C                                                                               SUNSPL  26
C              EXTERNALS                                                        SUNSPL  27
C              ---------                                                        SUNSPL  28
C                                                                               SUNSPL  29
C              MAXDIR                                                           SUNSPL  30
C                                                                               SUNSPL  31
      INTEGER IDOY                                                              SUNSPL  32
      REAL COSZIN,COSZAV,SOLHOR,DIRH,DIRN,DIF                                   SUNSPL  33
C                                                                               SUNSPL  34
      COMMON  /CONST/    DTOR, PIOVR2, PIOVR4, PIOV12, IBLNK                    /CONST/  2
C                                                                               /CONST/  3
C                                                                               SUNSPL  36
      DIRH = 0.                                                                 SUNSPL  37
      DIRN = 0.                                                                 SUNSPL  38
      DIF = 0.                                                                  SUNSPL  39
      IF (SOLHOR.EQ.0.) GO TO 1000                                              SUNSPL  40
      IF (COSZIN.EQ.0.) GO TO 1000                                              SUNSPL  41
C                                                                               SUNSPL  42
C              GET SOLAR CONSTANT.  THIS FORMULA IS FROM DUFFIE AND             SUNSPL  43
C              BECKMAN, PAGE 7.                                                 SUNSPL  44
      SOLCON = 436.8*(1. + 0.033*COS(DTOR*360.*FLOAT(IDOY)/365.))               SUNSPL  45
C              GET HOUR ANGLE AT UPPER AND LOWER HOUR BIN EDGE                  SUNSPL  46
C              THIS IS LOCAL TIME!                                              SUNSPL  47
C              EXTRATERRESTRIAL SOLAR HORIZONTAL                                SUNSPL  48
      SOLEXH   = SOLCON*COSZIN                                                  SUNSPL  49
C              K sub T IS THE RATIO OF TERRESTRIAL TO EXTRATERRESTRIAL SOLAR    SUNSPL  50
      RKT = AMIN1(SOLHOR/SOLEXH,0.9)                                            SUNSPL  51
C              GET DIFFUSE COMPONENT FROM ERBS, KLEIN, AND DUFFIE               SUNSPL  52
C              CORRELATION                                                      SUNSPL  53
      IF (RKT .LE. 0.22)  DIF = SOLHOR*(1.-0.09*RKT)                            SUNSPL  54
      RKT2   = RKT*RKT                                                          SUNSPL  55
      IF ((RKT .GT. 0.22) .AND. (RKT .LE. 0.8))  DIF = SOLHOR*                  SUNSPL  56
     1   (.9511-.1604*RKT+4.388*RKT2-16.638*RKT*RKT2+12.336*RKT2*RKT2)          SUNSPL  57
      IF (RKT .GT. 0.8)  DIF = 0.165*SOLHOR                                     SUNSPL  58
C              DIRECT HORIZONTAL                                                SUNSPL  59
      DIRH   = AMAX1(0.,SOLHOR-DIF)                                             SUNSPL  60
C              DIRECT NORMAL                                                    SUNSPL  61
      DIRN = DIRH/COSZAV                                                        SUNSPL  62
C              CHECK FOR MAXIMUM DIRECT NORMAL                                  SUNSPL  63
      CALL MAXDIR(COSZAV,SOLCON,DIRMAX)                                         SUNSPL  64
      DIRN = AMIN1(DIRN,DIRMAX)                                                 SUNSPL  65
 1000 CONTINUE                                                                  SUNSPL  66
      RETURN                                                                    SUNSPL  67
      END                                                                       SUNSPL  68
      SUBROUTINE SUNUD(DEABC,ELT,ELNG,NT,SRT,SST)                               SUNUD    2
C                                                                               SUNUD    3
C              THIS SUBROUTINE RETURNS THE SUNRISE AND SUNSET HOURS             SUNUD    4
C              GIVEN SUN PARAMETERS, LATITUDE, LONGITUDE, AND TIME ZONE.        SUNUD    5
C                                                                               SUNUD    6
C              'DEABC' = 5 SUN PARAMETERS RETURNED FROM 'SUNPRM'                SUNUD    7
C              'ELT'   = LATITUDE                                               SUNUD    8
C              'ELNG'  = LONGITUDE                                              SUNUD    9
C              'NT'    = TIME ZONE                                              SUNUD   10
C              'SRT'   = SUNRISE TIME                                           SUNUD   11
C              'SST'   = SUNSET TIME                                            SUNUD   12
C                                                                               SUNUD   13
      DIMENSION  DEABC(5)                                                       SUNUD   14
C                                                                               SUNUD   15
      TZN    = FLOAT(NT)                                                        SUNUD   16
      CSHP   = -TAN(.0174533*ELT)*TAN(DEABC(1))                                 SUNUD   17
      HP     = ACOS(CSHP)                                                       SUNUD   18
      Y      = HP*12.0/3.14159                                                  SUNUD   19
      SRT    = 12.0 - Y - DEABC(2) - TZN + ELNG/15.0                            SUNUD   20
      SST    = 24.0 - SRT                                                       SUNUD   21
      RETURN                                                                    SUNUD   22
      END                                                                       SUNUD   23
      SUBROUTINE  SUN1                                                          SUN1     2
C                                                                               SUN1     3
C              CALCULATES DAILY DATA ON SOLAR RADIATION                         SUN1     4
C                                                                               SUN1     5
      COMMON  /LOCALD/   STALAT,STALON,SSTALA,CSTALA,TSTALA,HRSLON              /LOCALD/ 2
C                                                                               /LOCALD/ 3
      COMMON /SUND/ISUNUP,GUNDOG,HORANG,TDECLN,EQTIME,SOLCON,                   /SUND/   2
     1        ATMEXT,SKYDFF,RAYCOS(3),RDN,BSUN,DECLN,CD,SD,                     /SUND/   3
     2        FSUNUP,CLDCOV,RDNCC,BSCC,DIFSOL,RTOT(4),SKYA                      /SUND/   4
      COMMON /TIM/ IDOY                                                         /TIME/   2
C                                                                               SUN1     9
C              GET SIN,COS OF DAY OF YEAR/365                                   SUN1    10
      C1     = COS( .01721*FLOAT( IDOY ) )                                      SUN1    11
      S1     = SIN( .01721*FLOAT( IDOY ) )                                      SUN1    12
      S2     = 2.*S1*C1                                                         SUN1    13
      C2     = C1*C1-S1*S1                                                      SUN1    14
      C3     = C1*C2-S1*S2                                                      SUN1    15
      S3     = C1*S2+S1*C2                                                      SUN1    16
C              CALC TANGENT OF DECLINATION ANGLE                                SUN1    17
      DECLN = .00527-.4001*C1-.003996*C2-.004240*C3+.0672*S1                    WTH2    68
C              CALC EQUATION OF TIME                                            SUN1    19
      EQTIME = .696E-4+.706E-2*C1-.0533*C2-.157E-2*C3-.122*S1-.156*S2           SUN1    20
     1         -.556E-2 * S3                                                    SUN1    21
C              CALC SOLAR CONSTANT                                              SUN1    22
      SOLCON = 368.44+24.52*C1-1.14*C2-1.09*C3+.58*S1-.18*S2+.28*S3             SUN1    23
C              IF SOUTHERN HEMISPHERE, SHIFT AIRMASS AND ATMOSPHERIC            SUN1    24
C              EXTINCTION CURVES BY 6 MONTHS.                                   SUN1    25
      IF (STALAT .GE. 0.)  GO TO 10                                             SUN1    26
      C1     = -C1                                                              SUN1    27
      S1     = -S1                                                              SUN1    28
      C3     = -C3                                                              SUN1    29
      S3     = -S3                                                              SUN1    30
   10 CONTINUE                                                                  SUN1    31
C              CALC ATMOSPHERIC EXTINCTION COEFFICIENT                          SUN1    32
      ATMEXT = .1717-.0344*C1+.0032*C2+.0024*C3-.0043*S1-.0008*S3               SUN1    33
C              CALCULATE SKY DIFFUSIVITY                                        SUN1    34
      SKYDFF = .0905-.0410*C1+.0073*C2+.0015*C3-.0034*S1+.0004*S2               SUN1    35
     1         - .0006*S3                                                       SUN1    36
C              CALC HOUR ANGLE OF SUNRISE                                       SUN1    37
      GUNDOG = ACOS( -TSTALA*TDECLN )                                           SUN1    38
      TDECLN  = TAN(DECLN)                                                      WTH2    69
      CD     = COS(DECLN)                                                       SUN1    40
      SD     = SIN(DECLN)                                                       SUN1    41
      RETURN                                                                    SUN1    42
      END                                                                       SUN1    43
      SUBROUTINE  SUN3R                                                         SUN3R    2
C                                                                               SUN3R    3
C              CALCULATES SOLAR DATA WHICH DEPENDS                              SUN3R    4
C              ON ORIENTATION OF A SURFACE                                      SUN3R    5
C                                                                               SUN3R    6
C              THIS PROGRAM CORRESPONDS TO ROUTINE  S U N 3   IN THE            SUN3R    7
C                LOADS PROGRAM.  IT WAS REVISED TO ENABLE CALCULATION           SUN3R    8
C                OF ADDITIONAL SOLAR RADIATION PARAMETERS IN THE                SUN3R    9
C                STATISTICS PORTION OF THE WEATHER PROCESSOR.                   SUN3R   10
C                                                                               SUN3R   11
      COMMON /SUND/ISUNUP,GUNDOG,HORANG,TDECLN,EQTIME,SOLCON,                   /SUND/   2
     1        ATMEXT,SKYDFF,RAYCOS(3),RDN,BSUN,DECLN,CD,SD,                     /SUND/   3
     2        FSUNUP,CLDCOV,RDNCC,BSCC,DIFSOL,RTOT(4),SKYA                      /SUND/   4
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /WEATHD/   IWDID(5),FIRSTW,LRECX,IWSOL,WLONG,WLAT,IWTZN,          /WEATHD/ 2
     1                   CLRNES,TGRND,IWYR,WBT,DBT,PATM,CLDAMT,ISNOW,           /WEATHD/ 3
     2                   IRAIN,IWNDDR,HUMRAT,DENSTY,ENTHAL,SOLRAD,              /WEATHD/ 4
     3                   DIRSOL,ICLDTY,WNDSPD,IRFCP,DPT                         /WEATHD/ 5
      LOGICAL FIRSTW                                                            /WEATHD/ 6
      DIMENSION          CALC(16)                                               /WEATHD/ 7
      EQUIVALENCE        (WBT,CALC(1))                                          /WEATHD/ 8
C                                                                               /WEATHD/ 9
C                                                                               SUN3R   15
      DIMENSION SWA(4),CWA(4)                                                   SUN3R   16
      DATA  GAMMA,SWT,GNDREF,FFG/0.0,1.0,.2,.5/                                 -092    15
      DATA SWA,CWA/0.0,1.0,0.0,-1.0,1.0,0.0,-1.0,0.0/                           SUN3R   18
C                                                                               SUN3R   19
C              CALC DIFFUSE RADIATION REFLECTED FROM GROUND                     SUN3R   20
      BG     =  GNDREF  * ( BSCC + RDNCC*RAYCOS(3) )                            SUN3R   21
      IF (IWSOL.GE.6)  BG=GNDREF*SOLRAD                                         SUN3R   22
C              CALCULATE ETA, THE COSINE OF THE ANGLE BETWEEN                   SUN3R   23
C              THE SOLAR DIRECTION AND THE SURFACE OUTWARD                      SUN3R   24
C              NORMAL.  LOOP OVER FOUR SURFACES CORRESPONDING TO                SUN3R   25
C              NORTH,SOUTH,EAST AND WEST.                                       SUN3R   26
C                                                                               SUN3R   27
      DO 5120 IK=1,4                                                            SUN3R   28
      RTOT(IK)=0.0                                                              SUN3R   29
      RDIR   = 0.0                                                              SUN3R   30
      RDIF   = 0.0                                                              SUN3R   31
      ETA    = (RAYCOS(1)*SWA(IK)+RAYCOS(2)*CWA(IK))*SWT+RAYCOS(3)*GAMMA        SUN3R   32
C              CALCULATE SKY FORM FACTOR                                        SUN3R   33
      FFS    = .45                                                              SUN3R   34
      IF (ETA .GE. -.2) FFS=.55+.437*ETA+.313*ETA*ABS(ETA)                      SUN3R   35
C              CALCULATE DIRECT SOLAR RADIATION.                                SUN3R   36
C              ETA < 0 MEANS NO DIRECT SOLAR                                    SUN3R   37
      RDIR   = 0.0                                                              SUN3R   38
      IF (ETA .GT. 0.0) RDIR = RDNCC*ETA                                        SUN3R   39
C              CALCULATE DIFFUSE SOLAR RADIATION.                               SUN3R   40
      RDIF   = FFS*BSCC + FFG*BG                                                SUN3R   41
C              CALCULATE TOTAL SOLAR.                                           SUN3R   42
      RTOT(IK)   = RDIR + RDIF                                                  SUN3R   43
 5120 CONTINUE                                                                  SUN3R   44
      RETURN                                                                    SUN3R   45
      END                                                                       SUN3R   46
      SUBROUTINE  SWAP(IARRAY,IDIM,I,J)                                         SWAP     2
C                                                                               SWAP     3
C              THIS ROUTINE SWAPS THE VALUES IN AN ARRAY                        SWAP     4
C                                                                               SWAP     5
C                                                                               SWAP     6
      DIMENSION  IARRAY(IDIM,336)                                               SWAP     7
C                                                                               SWAP     8
      DO 1000 K = 1,IDIM                                                        SWAP     9
      IT     = IARRAY(K,I)                                                      SWAP    10
      IARRAY(K,I) = IARRAY(K,J)                                                 SWAP    11
      IARRAY(K,J) = IT                                                          SWAP    12
 1000 CONTINUE                                                                  SWAP    13
      RETURN                                                                    SWAP    14
      END                                                                       SWAP    15
      SUBROUTINE TIMDAT ( JTIME, JDATE )                                        WTHWF   34
C                                                                               WTHWF   35
C              RETURNS TIME AND DATE IN 3A4                                     WTHWF   36
C                                                                               WTHWF   37
      CHARACTER*12 JDATE,JTIME                                                  WTHWF   38
      COMMON  /MONTHC/   BEFORE(12),MDAYS(12),MNAMES(12)                        /MONTHC/ 2
      INTEGER            BEFORE                                                 /MONTHC/ 3
C                                                                               /MONTHC/ 4
      include 'fsublib.fi'                                                      WTHWF   40
C                                                                               WTHWF   41
      INTEGER*2  IDM,IDD,IDY,IT(4)                                              WTHWF   42
      CALL GETDAT (IDY,IDM,IDD)                                                 WTHWF   43
      CALL GETTIM (IT(1),IT(2),IT(3),IT(4))                                     WTHWF   44
      WRITE(JDATE(:12),2000) IDD,MNAMES(IDM),IDY                                WTHWF   45
      WRITE(JTIME(:12),2001) (IT(i),i=1,3)                                      WTHWF   46
 2000 FORMAT(1X,I2,1X,A3,1X,I4)                                                 WTHWF   47
 2001 FORMAT(' at ',I2.2,':',I2.2,'.',I2.2)                                     WTHWF   48
      RETURN                                                                    WTHWF   49
      END                                                                       WTHWF   50
      SUBROUTINE TMYDCD                                                         TMYDCD   2
C                                                                               TMYDCD   3
C              THIS ROUTINE DECODES A DAY OF TMY DATA STARTING WITH THE         TMYDCD   4
C              FIRST CARD OF THE STATION 'ISTAT'                                TMYDCD   5
C                                                                               TMYDCD   6
      COMMON  /DAYPAR/   STSD,CTCD,SSS,CSC,HUNDOG,EQTIME,SOLCON,                /DAYPAR/ 2
     1                   TMSHFT,ISHFTH,WNXTHR,WPRVHR                            /DAYPAR/ 3
C                                                                               /DAYPAR/ 4
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /FILTYP/   IFTYP,IWSZ,ITPTYP                                      /FILTYP/ 2
C                                                                               /FILTYP/ 3
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /LOCALD/   STALAT,STALON,SSTALA,CSTALA,TSTALA,HRSLON              /LOCALD/ 2
C                                                                               /LOCALD/ 3
      COMMON  /PACKEC/   NTZ,ISTAT,XLAT,XLONG,                                  /PACKEC/ 2
     1                   IYR,INTINT,DDBT,IBEGH,NBS,IDOYSH(5),ISSHFT(5),         /PACKEC/ 3
     2                   ISHFT,ALT,SKYCOV(12),ILOC                              073a     1
C                                                                               /PACKEC/ 5
      COMMON  /PARAMS/   STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 2
      LOGICAL            STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 3
C                                                                               /PARAMS/ 4
      COMMON  /PCKINT/   KYR,KSTAT,CN(12),TG(12),KTIM,KARD(500),KYRL            -089     1
C                                                                               /PCKINT/ 3
      COMMON  /RAWDAT/   IDRY(24),IWET(24),IDEW(24),IPRESS(24),                 /RAWDAT/ 2
     1                   IWNDSP(24),ICLAMT(24),ISOL(24),IDN(24),                /RAWDAT/ 3
     2                   IWNDIR(24),ICLTY(24),IRN(24),ISN(24),                  CD144S   1
     3                   IOPQCA(24),IRELH(24),ICLTY1(24)                        CD144S   2
      DIMENSION IRAW(24,14)                                                     CD144S   3
      EQUIVALENCE        (IDRY(1),IRAW(1,1))                                    /RAWDAT/ 6
      COMMON /LSTIME/   LSTHRS(24)                                              /RAWDAT/ 7
C                                                                               /RAWDAT/ 8
      COMMON  /REPORC/   IBEGM,IENDM                                            /REPORC/ 2
C                                                                               /REPORC/ 3
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /TMYME/    IM,KDOM,IH                                             /TMYME/  2
C                                                                               /TMYME/  3
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
      DIMENSION HTOTL(24),                                                      TMYDCD  20
     1          HDN(24),                                                        TMYDCD  21
     2          TMYDNR(24),                                                     TMYDCD  22
     3          TMYDFR(24),                                                     TMYDCD  23
     4          TMYSYR(24),                                                     TMYDCD  24
     5          TMYETR(24)                                                      TMYDCD  25
      DIMENSION KW(8)                                                           TMYDCD  26
      DATA NOT2,NOT3,NOT4,NOT5/99,999,9999,99999/                               TMYDCD  28
      DATA  WBD      /0.0/                                                      TMYDCD  29
      DATA  HTOTL,HDN /48*0.0/                                                  TMYDCD  30
C                                                                               TMYDCD  31
C                                                                               TMYDCD  32
 3000 CONTINUE                                                                  TMYDCD  33
      IF (IDAY .EQ. IDAYL)  RETURN                                              TMYDCD  34
      DO 1040 IH=1,24                                                           TMYDCD  35
C              READ NEXT HOUR AND CONVERT TMY DATA                              TMYDCD  36
      IF (ITPTYP .EQ. 7)  GO TO 1005                                            TMYDCD  37
      READ(INWTH,9010,END=6000) KSTAT,KYR,IM,KDOM,KSTHRS,KSTMTS,                TMYDCD  38
     .  KLTHRS,KLTMTS,KETIRN,KDNIRN,KDFIRN,KSYIRN,LHR,(KW(I),I=1,8),            WTH2    70
     .  KPRESS,KDB,KDP,KWNDIR,KWNDSP,KCOV,KOPQ                                  TMYDCD  40
 9010 FORMAT (I5,7I2,I4,2(1X,I4),21X,I4,12X,I2,13X,                             TMYDCD  41
     .8I1,5X,I5,2I4,I3,I4,2I2)                                                  TMYDCD  42
      IF (KSTAT .NE. ISTAT) GO TO 3000                                          TMYDCD  43
      IF (IM    .LT. IBEGM) GO TO 3000                                          TMYDCD  44
      GO TO 1009                                                                TMYDCD  45
C              DECODE FOR SOLMET                                                TMYDCD  46
 1005 IF (IYR .EQ. IUNDEF)  GO TO 6000                                          TMYDCD  47
      READ(INWTH,9020,END=6000) KSTAT,KYR,IM,KDOM,KSTHRS,KSTMTS,                TMYDCD  48
     .  KLTHRS,KLTMTS,KETIRN,KDNIRN,KNFIRN,KSYIRN,LHR,(KW(I),I=1,8),            TMYDCD  49
     .  KPRESS,KDB,KDP,KWNDIR,KWNDSP,KCOV,KOPQ                                  TMYDCD  50
9020  FORMAT( 4X,I5,7I2,I4,2(1X,I4),21X,I4,12X,I2,13X,8I1,5X,I5                 TMYDCD  51
     .       ,2I4,I3,I4,2I2 )                                                   TMYDCD  52
      IF (KSTAT .NE. ISTAT)  GO TO 3000                                         TMYDCD  53
      KYR    = KYR + 1900                                                       TMYDCD  54
      IF (KYR .NE. IYR)  GO TO 3000                                             TMYDCD  55
      IF (FIRST.AND.IM.NE.IBEGM)  GO TO 3000                                    TMYDCD  56
 1009 CONTINUE                                                                  TMYDCD  57
      FIRST  = .FALSE.                                                          TMYDCD  58
C              INITIALIZE SNOW AND RAIN FLAGS                                   TMYDCD  59
      LSTHRS(IH) = LHR                                                          TMYDCD  60
      IRN(IH) = 0                                                               TMYDCD  61
      ISN(IH) = 0                                                               TMYDCD  62
      TMYDNR(IH) = .08807 * FLOAT(KDNIRN)                                       TMYDCD  63
      IF (KDNIRN .EQ. NOT4)  TMYDNR(IH) = 0.                                    TMYDCD  64
      TMYDFR(IH) = .08807 * FLOAT(KDFIRN)                                       TMYDCD  65
      IF (KDFIRN .EQ. NOT4)  TMYDFR(IH) = 0.                                    TMYDCD  66
      TMYSYR(IH) = .08807 * FLOAT(KSYIRN)                                       TMYDCD  67
      IF (KSYIRN .EQ. NOT4)  TMYSYR(IH) = 0.                                    TMYDCD  68
      TMYETR(IH) = .08807 * FLOAT(KETIRN)                                       TMYDCD  69
      IF (KETIRN .EQ. NOT4)  TMYETR(IH) = 0.                                    TMYDCD  70
      IF (KDB .EQ. NOT4) GO TO 1010                                             TMYDCD  71
      TDB    = .18 * FLOAT(KDB) + 32.                                           TMYDCD  72
      IDRY(IH) = IFIX(TDB + .5)                                                 TMYDCD  73
      GO TO 1011                                                                TMYDCD  74
 1010 IDRY(IH) = IUNDEF                                                         TMYDCD  75
 1011 IF (KDP .EQ. NOT4)  GO TO 1012                                            TMYDCD  76
      IF (KDP .GT. KDB)   KDP = KDB                                             TMYDCD  77
      TDP    = .18 * FLOAT(KDP) + 32.                                           TMYDCD  78
      IDEW(IH) = IFIX(TDP + .5)                                                 TMYDCD  79
      GO TO 1013                                                                TMYDCD  80
 1012 IDEW(IH) = IUNDEF                                                         TMYDCD  81
 1013 IF (KPRESS .EQ. NOT5)  GO TO 1014                                         TMYDCD  82
      BP     = .002953 * FLOAT(KPRESS)                                          TMYDCD  83
      IPRESS(IH) = IFIX(100.*BP + .5)                                           TMYDCD  84
      GO TO 1015                                                                TMYDCD  85
 1014 IPRESS(IH) = IUNDEF                                                       TMYDCD  86
 1015 IF (KDB .EQ. NOT4 .OR. KDP .EQ. NOT4 .OR. KPRESS .EQ. NOT5)               TMYDCD  87
     1  GO TO 1016                                                              TMYDCD  88
      TWB    = SLVWBT(BP,TDB,TDP,TDP-WBD)                                       TMYDCD  89
      WBD    = TDB - TWB                                                        TMYDCD  90
      IWET(IH) = IFIX(TWB + .5)                                                 TMYDCD  91
      GO TO 1017                                                                TMYDCD  92
 1016 IWET(IH) = IUNDEF                                                         TMYDCD  93
 1017 CONTINUE                                                                  TMYDCD  94
      IF ((KW(7).GT.0 .AND. KW(7).LT.9) .OR. (KW(6).GT.0 .AND. KW(6).LT.        TMYDCD  95
     1 7) .OR. (KW(3).GT.0 .AND. KW(3).LT.9))  IRN(IH) = 1                      TMYDCD  96
      IF ((KW(5).GT.0 .AND. KW(5).LT.9)                                         TMYDCD  97
     1 .OR. (KW(4).GT.0 .AND. KW(4).LT.7))  ISN(IH) = 1                         TMYDCD  98
      IWNDSP(IH) = IFIX(FLOAT(KWNDSP) * .1944 + .5)                             TMYDCD  99
      IF (KWNDSP .EQ. NOT4)  IWNDSP(IH) = IUNDEF                                TMYDCD 100
      IF (KWNDIR .EQ. NOT3)  IWNDIR(IH) = IUNDEF                                TMYDCD 101
C              SET WIND DIRECTION TO ^0-15[                                     TMYDCD 102
      IF (IWNDSP(IH) .EQ. 0)  IWNDIR(IH) = 0                                    TMYDCD 103
      IF (IWNDSP(IH) .EQ. 0)  GO TO 1020                                        TMYDCD 104
      IF (IWNDIR(IH) .EQ. IUNDEF)  GO TO 1020                                   TMYDCD 105
      IWNDIR(IH) = INT(FLOAT(KWNDIR)*4.444444E-2 + 0.5)                         TMYDCD 106
      IF (IWNDIR(IH) .EQ. 16)  IWNDIR(IH) = 0                                   TMYDCD 107
 1020 CONTINUE                                                                  TMYDCD 108
      IF (KOPQ .EQ. 0)  GO TO 1030                                              TMYDCD 109
      IF (KOPQ .EQ. NOT2)  GO TO 1030                                           TMYDCD 110
C              KOPQ .GT. 0                                                      TMYDCD 111
      ICLTY(IH)  = 2                                                            TMYDCD 112
      ICLAMT(IH) = KOPQ                                                         TMYDCD 113
      GO TO 1040                                                                TMYDCD 114
C              KOPQ ZERO OR NOT THERE                                           TMYDCD 115
 1030 ICLTY(IH) = 2                                                             TMYDCD 116
      IF (KOPQ .EQ. 0)  ICLTY(IH) = 1                                           TMYDCD 117
      IF (KCOV .EQ. NOT2)  ICLTY(IH) = IUNDEF                                   TMYDCD 118
      ICLAMT(IH) = KCOV                                                         TMYDCD 119
      IF (KCOV .EQ. NOT2)  ICLAMT(IH) = IUNDEF                                  TMYDCD 120
      ICLTY1(IH) = IUNDEF                                                       TMYDCD 121
 1040 CONTINUE                                                                  TMYDCD 122
      CALL DAYPR(IM,KDOM,KYR,NTZ)                                               TMYDCD 123
      CALL HSHIFT(TMYSYR,TMYDNR,TMYDFR,TMYETR,HTOTL,HDN)                        TMYDCD 124
      DO 1080 IH = 1,24                                                         TMYDCD 125
      ISOL(IH) = IFIX(HTOTL(IH) + .5)                                           TMYDCD 126
      IDN(IH)  = IFIX(HDN(IH)   + .5)                                           TMYDCD 127
 1080 CONTINUE                                                                  TMYDCD 128
      IDAYL  = KDOM                                                             TMYDCD 129
      RETURN                                                                    TMYDCD 130
 6000 IEOF   = .TRUE.                                                           TMYDCD 131
      RETURN                                                                    TMYDCD 132
      END                                                                       TMYDCD 133
      SUBROUTINE TMY2DCD                                                        TMY2DCD  2
C                                                                               TMY2DCD  3
C              Decode one day of weather data in TMY2 format and store          TMY2DCD  4
C              the result in the common block /RAWDAT/.                         TMY2DCD  5
C                                                                               TMY2DCD  6
      COMMON  /DAYPAR/   STSD,CTCD,SSS,CSC,HUNDOG,EQTIME,SOLCON,                /DAYPAR/ 2
     1                   TMSHFT,ISHFTH,WNXTHR,WPRVHR                            /DAYPAR/ 3
C                                                                               /DAYPAR/ 4
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /LOCALD/   STALAT,STALON,SSTALA,CSTALA,TSTALA,HRSLON              /LOCALD/ 2
C                                                                               /LOCALD/ 3
      COMMON  /PACKEC/   NTZ,ISTAT,XLAT,XLONG,                                  /PACKEC/ 2
     1                   IYR,INTINT,DDBT,IBEGH,NBS,IDOYSH(5),ISSHFT(5),         /PACKEC/ 3
     2                   ISHFT,ALT,SKYCOV(12),ILOC                              073a     1
C                                                                               /PACKEC/ 5
      COMMON  /PARAMS/   STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 2
      LOGICAL            STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 3
C                                                                               /PARAMS/ 4
      COMMON  /PCKINT/   KYR,KSTAT,CN(12),TG(12),KTIM,KARD(500),KYRL            -089     1
C                                                                               /PCKINT/ 3
      COMMON  /RAWDAT/   IDRY(24),IWET(24),IDEW(24),IPRESS(24),                 /RAWDAT/ 2
     1                   IWNDSP(24),ICLAMT(24),ISOL(24),IDN(24),                /RAWDAT/ 3
     2                   IWNDIR(24),ICLTY(24),IRN(24),ISN(24),                  CD144S   1
     3                   IOPQCA(24),IRELH(24),ICLTY1(24)                        CD144S   2
      DIMENSION IRAW(24,14)                                                     CD144S   3
      EQUIVALENCE        (IDRY(1),IRAW(1,1))                                    /RAWDAT/ 6
      COMMON /LSTIME/   LSTHRS(24)                                              /RAWDAT/ 7
C                                                                               /RAWDAT/ 8
      COMMON  /REPORC/   IBEGM,IENDM                                            /REPORC/ 2
C                                                                               /REPORC/ 3
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /TMYME/    IM,KDOM,IH                                             /TMYME/  2
C                                                                               /TMYME/  3
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
C                                                                               TMY2DCD 19
      INTEGER KW(10)                                                            TMY2DCD 20
      CHARACTER*132 LINE                                                        -037     1
      REAL WBD                                                                  TMY2DCD 21
      SAVE WBD                                                                  TMY2DCD 22
C                                                                               TMY2DCD 23
      WBD = 0.0                                                                 TMY2DCD 24
C                                                                               TMY2DCD 25
 3000 CONTINUE                                                                  TMY2DCD 26
      IF (IDAY.EQ.IDAYL) RETURN                                                 TMY2DCD 27
      DO 1000 IH=1,24                                                           TMY2DCD 28
C              Read one hour of data                                            TMY2DCD 29
      READ(INWTH,9010,END=6000,ERR=9020) KYR,IM,KDOM,KHR,KSOL,KDN,KCOV,         -037     2
     $     KOPQ,KDB,KDP,KPRESS,KWNDIR,KWNDSP,(KW(I),I=1,10)                     -037     3
 9010 FORMAT(1X,4I2,8X,I4,2X,I4,32X,I2,2X,I2,2X,I4,2X,I4,7X,I4,2X,I3,           TMY2DCD 32
     1       2X,I3,15X,10I1)                                                    TMY2DCD 33
      GOTO 9050                                                                 -037     4
C              Bad line ... write it out so it can be seen                      -037     5
 9020   BACKSPACE INWTH                                                         -037     6
        READ (INWTH,9030) LINE                                                  -037     7
        WRITE(OUTFIL,9040) LINE,(i*10,i=1,13),                                  -037     8
     $    LINE(2:3), LINE(4:5), LINE(6:7), LINE(8:9),                           -037     9
     $    LINE(18:21), LINE(24:27), LINE(60:61), LINE(64:65),                   -037    10
     $    LINE(68:71), LINE(74:77), LINE(85:88),                                -037    11
     $    LINE(91:93), LINE(96:98),                                             -037    12
     $    (LINE(113+i:113+i),i=1,10)                                            -037    13
 9030   FORMAT (A132)                                                           -037    14
 9040   FORMAT (' *ERROR - Bad character in field on input weather ',           -037    15
     $          'data file:'/1X,A132/1X,13('123456789*')/1X,13(I10)/1X,         -037    16
     $'Year (2:3)      = ',A2,2X,' Month (4:5)     = ',A2,2X,                   -037    17
     $' Day (6:7)       = ',A2,2X,' Hour (8:9)      = ',A2,2X/1X,               -037    18
     $'Tot Sol (18:21) = ',A4,' DirNorm (24:27) = ',A4,                         -037    19
     $' Cover (60:61)   = ',A2,2X,' Opaque (64:65)  = ',A2,2X/1X,               -037    20
     $'Dry Bulb (68:71)= ',A4,' Dew Pt (74:77)  = ',A4,                         -037    21
     $' AtmPress (85:88)= ',A4/1X,                                              -037    22
     $'Wind Dir (91:93)= ',A3,1X,' Wind Spd (96:98)= ',A3,1X/1X,                -037    23
     $'Ten conditions digits (114:123)= ',10(A1,1X))                            -037    24
 9050 CONTINUE                                                                  -037    25
                                                                                -037    26
      IF (IM.LT.IBEGM) GO TO 3000                                               TMY2DCD 34
      FIRST = .FALSE.                                                           TMY2DCD 35
C              Initialize snow and rain flags                                   TMY2DCD 36
      IRN(IH) = 0                                                               TMY2DCD 37
      ISN(IH) = 0                                                               TMY2DCD 38
C              Fill the total horizontal and direct normal solar                TMY2DCD 39
      ISOL(IH) = IROUND(0.3170*FLOAT(KSOL))                                     TMY2DCD 40
      IDN(IH) = IROUND(0.3170*FLOAT(KDN))                                       TMY2DCD 41
C              Dry bulb and dew point temperatures.  KDB and KDP are            TMY2DCD 42
C              in tenths of degrees C.                                          TMY2DCD 43
      TDB = .18*FLOAT(KDB) + 32.                                                TMY2DCD 44
      TDP = .18*FLOAT(KDP) + 32.                                                TMY2DCD 45
      IDRY(IH) = IROUND(TDB)                                                    TMY2DCD 46
      IDEW(IH) = IROUND(TDP)                                                    TMY2DCD 47
C              Atmospheric pressure (BP is in inches of Hg)                     TMY2DCD 48
C              .029529 is 29.921 * .0009869                                     TMY2DCD 49
      BP = .029529*FLOAT(KPRESS)                                                TMY2DCD 50
      IPRESS(IH) = IROUND(100.*BP)                                              TMY2DCD 51
C              Solve for the wet bulb temperature                               TMY2DCD 52
      TWB = SLVWBT(BP,TDB,TDP,TDB-WBD)                                          TMY2DCD 53
      WBD = TDB - TWB                                                           TMY2DCD 54
      IWET(IH) = IROUND(TWB)                                                    TMY2DCD 55
C              Windspeed (KWNDSP is in tenths of meters per second;             TMY2DCD 56
C              DOE-2 wants knots).                                              TMY2DCD 57
      IWNDSP(IH) = IROUND(.1944*FLOAT(KWNDSP))                                  TMY2DCD 58
C              Wind direction.  Change from degrees to compass points.          TMY2DCD 59
      IWNDIR(IH) = IROUND(FLOAT(KWNDIR)*4.444444E-2)                            TMY2DCD 60
      IF (IWNDIR(IH).EQ.16)  IWNDIR(IH) = 0                                     TMY2DCD 61
C              Cloud Amount and cloud type.  For cloud amount we will           TMY2DCD 62
C              use the opaque cloud cover.  There is no cloud type              TMY2DCD 63
C              information on TMY2, so we use the DOE-2 default type            TMY2DCD 64
C              of 2.                                                            TMY2DCD 65
      ICLAMT(IH) = KOPQ                                                         TMY2DCD 66
      ICLTY(IH) = 2                                                             TMY2DCD 67
C              ICLTY1 is the weather service cloud type.  It is only            TMY2DCD 68
C              reported; it is never used by DOE-2.                             TMY2DCD 69
      ICLTY1(IH) = IUNDEF                                                       TMY2DCD 70
C              Rain and Snow flags                                              TMY2DCD 71
      IF (KW(1).EQ.9) THEN                                                      TMY2DCD 72
          IRN(IH) = IUNDEF                                                      TMY2DCD 73
          ISN(IH) = IUNDEF                                                      TMY2DCD 74
      ELSE                                                                      TMY2DCD 75
          IF (KW(2).LT.9 .OR. KW(3).LT.9 .OR. KW(4).LT.9) IRN(IH) = 1           TMY2DCD 76
          IF (KW(5).LT.9 .OR. KW(6).LT.9 .OR. KW(7).LT.9                        TMY2DCD 77
     1        .OR. KW(10).LT.9) ISN(IH) = 1                                     TMY2DCD 78
      END IF                                                                    TMY2DCD 79
 1000 CONTINUE                                                                  TMY2DCD 80
      IDAYL = KDOM                                                              TMY2DCD 81
      RETURN                                                                    TMY2DCD 82
 6000 IEOF = .TRUE.                                                             TMY2DCD 83
      RETURN                                                                    TMY2DCD 84
      END                                                                       TMY2DCD 85
      SUBROUTINE TRYDCD                                                         TRYDCD   2
C                                                                               TRYDCD   3
C              THIS ROUTINE DECODES A DAY OF TRY DATA STARTING WITH THE         TRYDCD   4
C              FIRST CARD OF THE STATION 'ISTAT'                                TRYDCD   5
      COMMON  /CONST/    DTOR, PIOVR2, PIOVR4, PIOV12, IBLNK                    /CONST/  2
C                                                                               /CONST/  3
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /FILTYP/   IFTYP,IWSZ,ITPTYP                                      /FILTYP/ 2
C                                                                               /FILTYP/ 3
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /LOCALD/   STALAT,STALON,SSTALA,CSTALA,TSTALA,HRSLON              /LOCALD/ 2
C                                                                               /LOCALD/ 3
      COMMON  /MONTHC/   BEFORE(12),MDAYS(12),MNAMES(12)                        /MONTHC/ 2
      INTEGER            BEFORE                                                 /MONTHC/ 3
C                                                                               /MONTHC/ 4
      COMMON  /PACKEC/   NTZ,ISTAT,XLAT,XLONG,                                  /PACKEC/ 2
     1                   IYR,INTINT,DDBT,IBEGH,NBS,IDOYSH(5),ISSHFT(5),         /PACKEC/ 3
     2                   ISHFT,ALT,SKYCOV(12),ILOC                              073a     1
C                                                                               /PACKEC/ 5
      COMMON  /PARAMS/   STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 2
      LOGICAL            STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 3
C                                                                               /PARAMS/ 4
      COMMON  /PCKINT/   KYR,KSTAT,CN(12),TG(12),KTIM,KARD(500),KYRL            -089     1
C                                                                               /PCKINT/ 3
      COMMON  /RAWDAT/   IDRY(24),IWET(24),IDEW(24),IPRESS(24),                 /RAWDAT/ 2
     1                   IWNDSP(24),ICLAMT(24),ISOL(24),IDN(24),                /RAWDAT/ 3
     2                   IWNDIR(24),ICLTY(24),IRN(24),ISN(24),                  CD144S   1
     3                   IOPQCA(24),IRELH(24),ICLTY1(24)                        CD144S   2
      DIMENSION IRAW(24,14)                                                     CD144S   3
      EQUIVALENCE        (IDRY(1),IRAW(1,1))                                    /RAWDAT/ 6
      COMMON /LSTIME/   LSTHRS(24)                                              /RAWDAT/ 7
C                                                                               /RAWDAT/ 8
      COMMON  /REPORC/   IBEGM,IENDM                                            /REPORC/ 2
C                                                                               /REPORC/ 3
      COMMON  /SKIPPY/   FIRSTS,NCALL                                           /SKIPPY/ 2
      LOGICAL            FIRSTS                                                 /SKIPPY/ 3
C                                                                               /SKIPPY/ 4
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
C                                                                               TRYDCD  20
      DIMENSION IRANGE(2,17), ITEMP(17), NOTTHR(17)                             TRYDCD  21
      DIMENSION ITOC(4),ICA(4),DEABC(5)                                         TRYDCD  23
      DIMENSION RKTS(24),DIRN(24),SOLHOR(24)                                    TRYDCD  24
      DIMENSION ISOLWY(24)                                                      TRYDCD  25
      REAL NEWSSM                                                               TRYDCD  26
      LOGICAL LEVDAT                                                            CD144S 176
C                                                                               TRYDCD  27
      DATA  IRANGE   / 6, 8,    9,11,   12,14,   21,24,                         TRYDCD  28
     1                18,20,   26,27,   15,17,   30,30,   28,29,                TRYDCD  29
     2               36,36,  34,35,  44,44,  42,43,  52,52,  50,51,             TRYDCD  30
     3                56,59, 60,63 /                                            TRYDCD  31
      DATA  NOTTHR   /999,999,999,9999,999,99,999,9,99,                         TRYDCD  32
     1                 9,99,9,99,9,99,9999,9999 /                               TRYDCD  33
      DATA  ITOC/0,0,0,0/,ICA/0,0,0,0/,ITCA/0/                                  TRYDCD  34
C                                                                               TRYDCD  35
      IF (.NOT. FIRSTS) GO TO 3000                                              TRYDCD  36
   3  READ (INWTH,5001,END=6) KSTAT,KYR,IM,ID,IH                                TRYDCD  37
 5001 FORMAT(I5,64X,I4,3I2)                                                     TRYDCD  38
      IF (IM .EQ. 1 .AND. ID .EQ. 1 .AND. IH .EQ. 0 ) GO TO 1                   TRYDCD  39
   6  REWIND INWTH                                                              TRYDCD  40
      GO TO 3000                                                                TRYDCD  41
    1 IF (KSTAT .NE. ISTAT) GO TO 2                                             TRYDCD  42
      BACKSPACE INWTH                                                           TRYDCD  43
      GO TO 3000                                                                TRYDCD  44
    2 ISKIP = 365                                                               TRYDCD  45
      IF (MOD(KYR,4) .EQ. 0 .AND. MOD(KYR,100) .NE. 0                           TRYDCD  46
     . .OR. MOD(KYR,400) .EQ. 0) ISKIP = ISKIP + 1                              TRYDCD  47
      BACKSPACE INWTH                                                           TRYDCD  48
      DO 4 III=1, ISKIP                                                         TRYDCD  49
    4 READ (INWTH,5003)                                                         TRYDCD  50
 5003 FORMAT(///////////////////////)                                           TRYDCD  51
      GO TO 3                                                                   TRYDCD  52
 3000 CONTINUE                                                                  TRYDCD  53
      IF (IDAY .EQ. IDAYL)  RETURN                                              TRYDCD  54
      IDOY   = BEFORE(IMNTH) + IDAY                                             TRYDCD  55
      IF (ITPTYP .NE. 8)  GO TO 1007                                            TRYDCD  56
      SUNSUM = 0.                                                               TRYDCD  57
      NEWSSM = 0.                                                               TRYDCD  58
      DO 1005 I=1,NBS                                                           TRYDCD  59
      IF (IDOY .NE. IDOYSH(I)) GO TO 1005                                       TRYDCD  60
      ISHFT = ISSHFT(I)                                                         TRYDCD  61
      IF (ISHFT .GT. 0)  ISHFT = ISHFT - 1                                      TRYDCD  62
 1005 CONTINUE                                                                  TRYDCD  63
 1007 CONTINUE                                                                  TRYDCD  64
      DO 1040 IH=1,24                                                           TRYDCD  65
C              REPEAT LAST HOUR OF YEAR IF WYEC                                 TRYDCD  66
      IF(ITPTYP.NE.8) GO TO 3100                                                TRYDCD  67
      IF ((IMNTH .EQ. 12) .AND. (IDAY .EQ. 31) .AND. (IH .EQ. 24))              TRYDCD  68
     1   GO TO 3200                                                             TRYDCD  69
 3100 CALL  GETCRD(INWTH,KARD,80)                                               TRYDCD  70
C                                                                               TRYDCD  71
C              CHECK FOR END-OF-FILE                                            TRYDCD  72
 3200 IF (IEOF) RETURN                                                          TRYDCD  73
C              CHECK FOR CORRECT STATION AND BEGINNING MONTH                    TRYDCD  74
      KSTAT  = NUMB(KARD,1,5)                                                   TRYDCD  75
      IF (KSTAT .NE. ISTAT)  GO TO 3000                                         TRYDCD  76
      IM     = NUMB(KARD,74,75)                                                 TRYDCD  77
      IF (IM .LT. IBEGM)  GO TO 3000                                            TRYDCD  78
C              THROW AWAY FIRST HOUR OF WYEC TAPES                              TRYDCD  79
      IF(FIRST.AND.ITPTYP.EQ.8) CALL GETCRD(INWTH,KARD,80)                      TRYDCD  80
      KYR    = NUMB(KARD,70,73)                                                 TRYDCD  81
      FIRST  = .FALSE.                                                          TRYDCD  82
C              INITIALIZE SNOW AND RAIN FLAGS                                   TRYDCD  83
      IRN(IH) = 0                                                               TRYDCD  84
      ISN(IH) = 0                                                               TRYDCD  85
      ITCA = 0                                                                  CD144S 177
      DO I=1,4                                                                  CD144S 178
        ITOC(I) = 0                                                             CD144S 179
        ICA(I) = 0                                                              CD144S 180
      END DO                                                                    CD144S 181
C              FILL IN VALUES FOR EACH HOUR                                     TRYDCD  86
      DO 1020 I = 1,17                                                          TRYDCD  87
      ITEMP(I) = NUMB(KARD,IRANGE(1,I),IRANGE(2,I))                             TRYDCD  88
 1020 CONTINUE                                                                  TRYDCD  89
      IW     = NUMB(KARD,25,25)                                                 TRYDCD  90
C              CHECK FOR UNFILLED FIELDS                                        TRYDCD  91
      DO 1030 I=1,7                                                             TRYDCD  92
      IF (ITEMP(I) .EQ. NOTTHR(I))  ITEMP(I) = IUNDEF                           TRYDCD  93
 1030 CONTINUE                                                                  TRYDCD  94
      IF (IW .EQ. 7 .OR. IW .EQ. 5) IRN(IH) = 1                                 TRYDCD  95
      IF (IW .EQ. 8) ISN(IH) = 1                                                TRYDCD  96
      IF (ITPTYP .EQ. 8)  GO TO 200                                             TRYDCD  97
      ISOL(IH) = ITEMP(16)                                                      TRYDCD  98
      IDN(IH)  = ITEMP(17)                                                      TRYDCD  99
      IF (ITEMP(16) .EQ. NOTTHR(16)) ISOL(IH) = IUNDEF                          TRYDCD 100
      IF (ITEMP(17) .EQ. NOTTHR(17)) IDN(IH) = IUNDEF                           TRYDCD 101
      IF (ITPTYP .EQ. 2) GO TO 300                                              TRYDCD 102
      LEVDAT = .TRUE.                                                           CD144S 182
      IF(ITEMP(6).EQ.IUNDEF) GO TO 131                                          TRYDCD 103
      ITCA   = ITEMP(6)                                                         TRYDCD 104
  131 DO 132 I=8,14,2                                                           TRYDCD 105
      IF(ITEMP(I).EQ.NOTTHR(I)) THEN                                            CD144S 183
        LEVDAT = .FALSE.                                                        CD144S 184
        GO TO 132                                                               CD144S 185
      END IF                                                                    CD144S 186
      ITOC(I/2-3)=ITEMP(I)                                                      TRYDCD 107
  132 CONTINUE                                                                  TRYDCD 108
      DO 133 I=9,15,2                                                           TRYDCD 109
      IF(ITEMP(I).EQ.NOTTHR(I)) THEN                                            CD144S 187
        LEVDAT = .FALSE.                                                        CD144S 188
        GO TO 133                                                               CD144S 189
      END IF                                                                    CD144S 190
      ICA(I/2-3)=ITEMP(I)                                                       TRYDCD 111
  133 CONTINUE                                                                  TRYDCD 112
      IF (ITPTYP.EQ.15) GO TO 150                                               073a    14
  134 CALL  CCFKS(IDOY,ITOC,ICA,ITCA,CCF,CC,P)                                  TRYDCD 113
      IF(IH.EQ.1)  CALL  SUNPRM(IDOY,DEABC)                                     TRYDCD 114
      CALL SUNCOS(IH,DEABC,COSZIN,COSZ,SALT,FSUN)                               -092    16
      CALL SUNCLR(IMNTH,DEABC,COSZ,HDNCLR,BSCLR,TOTCLR)                         073a    16
      HDN    = 0.                                                               TRYDCD 116
      HDHOR  = 0.                                                               TRYDCD 117
      HDFHOR = 0.                                                               TRYDCD 118
      IF(COSZ.GT.0.0) CALL  SOLAD(DEABC,P,CC,CCF,SALT,COSZ,                     TRYDCD 119
     1HDNCLR,BSCLR,HDN,HDHOR,HDFHOR)                                            TRYDCD 120
      IF(ITPTYP.NE.9) GO TO 200                                                 TRYDCD 121
C              SOLAR FOR KIMURA-STEPHENSON TRYS                                 TRYDCD 122
      ISOL(IH)=IFIX(HDHOR+HDFHOR)                                               TRYDCD 123
      IDN(IH)=IFIX(HDN)                                                         TRYDCD 124
      GO TO 300                                                                 TRYDCD 125
  150 X = 0.                                                                    073a    17
      IF (LEVDAT) THEN                                                          CD144S 191
        DO I=1,4                                                                CD144S 192
          IF (ITOC(I).EQ.8 .OR. ITOC(I).EQ.9) X = X + FLOAT(ICA(I))             CD144S 193
        END DO                                                                  CD144S 194
        COPQ = FLOAT(ITCA) - 0.5*X                                              CD144S 195
        COPQ = AMAX1(AMIN1(10.,COPQ),0.)                                        CD144S 196
        IOPQCA(IH) = IROUND(COPQ)                                               CD144S 197
      ELSE                                                                      CD144S 198
        IOPQCA(IH) = ITEMP(6)                                                   CD144S 199
      END IF                                                                    CD144S 200
      GO TO 300                                                                 073a    31
  200 CONTINUE                                                                  TRYDCD 126
C              STORE THE WYEC SOLAR DATA FOR THE DAY                            TRYDCD 127
      ISOLWY(IH) = ITEMP(16)                                                    TRYDCD 128
  300 CONTINUE                                                                  TRYDCD 129
      IRAW(IH,9) = ITEMP(7)                                                     TRYDCD 130
      DO 1035 I = 1,6                                                           TRYDCD 131
      IRAW(IH,I) = ITEMP(I)                                                     TRYDCD 132
 1035 CONTINUE                                                                  TRYDCD 133
      ICLTY(IH) = ITEMP(8)                                                      TRYDCD 134
      IF ((ITEMP(8) .EQ. NOTTHR(8)) .AND. (ITEMP(9) .EQ. NOTTHR(9)))            TRYDCD 135
     1 ICLTY(IH) = IUNDEF                                                       TRYDCD 136
 1040 CONTINUE                                                                  TRYDCD 137
      IF (ITPTYP .NE. 8)  GO TO 1049                                            TRYDCD 138
C              SOLAR FOR WYEC                                                   TRYDCD 139
C              CALCULATE RKT                                                    TRYDCD 140
      DO 1041 IH=1,24                                                           TRYDCD 141
      ISOL(IH) = 0                                                              TRYDCD 142
      IDN(IH) = 0                                                               TRYDCD 143
      RKTS(IH) = 0.                                                             TRYDCD 144
      SOLHOR(IH) = 0.                                                           TRYDCD 145
      DIRN(IH) = 0.                                                             TRYDCD 146
      IF (IH .EQ. 1)  CALL SUNPRM(IDOY,DEABC)                                   TRYDCD 147
C              GET HOUR ANGLE.  ASSUME SOLAR DATA IS BINNED                     TRYDCD 148
C              IN SOLAR TIME.                                                   TRYDCD 149
      UL     = FLOAT(IH) - 12.                                                  TRYDCD 150
      BL     = UL - 1.                                                          TRYDCD 151
C              SUNRISE AND SUNSET HOUR ANGLES                                   TRYDCD 152
      SSHA   = ACOS(-TAN(STALAT)*TAN(DEABC(1)))/PIOV12                          TRYDCD 153
      SRHA   = -SSHA                                                            TRYDCD 154
C              RESET BIN BOUNDARIES TO ALLOW FOR SUNRISE AND SET                TRYDCD 155
      IF ((UL .LE. SRHA) .OR. (BL .GE. SSHA))  GO TO 1041                       TRYDCD 156
      IF (SRHA .GT. BL)  BL = SRHA                                              TRYDCD 157
      IF (SSHA .LT. UL)  UL = SSHA                                              TRYDCD 158
      IF ((UL-BL) .LT. 0.02)  GO TO 1041                                        TRYDCD 159
      IF (ISOLWY(IH+ISHFT) .EQ. 0)  GO TO 1041                                  TRYDCD 160
      IF (ISOLWY(IH+ISHFT) .EQ. NOTTHR(16))  RKTS(IH) = IUNDEF                  TRYDCD 161
      IF (ISOLWY(IH+ISHFT) .EQ. NOTTHR(16))  GO TO 1041                         TRYDCD 162
      A      = SIN(DEABC(1))*SIN(STALAT)                                        TRYDCD 163
      B      = COS(DEABC(1))*COS(STALAT)                                        TRYDCD 164
C              GET SOLAR CONSTANT.  THIS FORMULA IS FROM DUFFIE AND             TRYDCD 165
C              BECKMAN, PAGE 7.                                                 TRYDCD 166
      SOLCON = 436.8*(1. + 0.033*COS(DTOR*360.*FLOAT(IDOY)/365.))               TRYDCD 167
C              INTEGRATE SOLAR Z DIREC. COSINE OVER BIN                         TRYDCD 168
      COSZIN = A*(UL-BL) + B*(SIN(PIOV12*UL)-SIN(PIOV12*BL))/PIOV12             TRYDCD 169
      SOLH   = SOLCON*COSZIN                                                    TRYDCD 170
      SOLHM  = FLOAT(ISOLWY(IH+ISHFT))*0.3686691                                TRYDCD 171
      SUNSUM = SUNSUM + SOLHM                                                   TRYDCD 172
      RKTS(IH) = AMIN1(SOLHM/SOLH,0.9)                                          TRYDCD 173
C              IF (IMNTH .NE. 5)  GO TO 1041                                    TRYDCD 174
C              PRINT 9004,IH,IDAY,MNAMES(IMNTH),ITEMP(16),ISHFT,IDOY            TRYDCD 175
 9004 FORMAT(1H ,9HIH     = ,I11,10X,9HIDAY   = ,I11,10X,9HMNAMES = ,           TRYDCD 176
     1       7X,A4,10X,9HITMP16 = ,I11/1H ,9HISHFT  = ,I11,10X,                 TRYDCD 177
     2       9HIDOY   = ,I11)                                                   TRYDCD 178
 1041 CONTINUE                                                                  TRYDCD 179
C              REBIN WYEC SOLAR DATA                                            TRYDCD 180
C              LOOP TO FILL IN MISSING SOLAR DATA                               TRYDCD 181
      PRKTS  = 0.                                                               TRYDCD 182
      IHRSP  = 0                                                                TRYDCD 183
      DO 1044 IH=1,24                                                           TRYDCD 184
      UL     = FLOAT(IH) - 12.                                                  TRYDCD 185
      BL     = UL - 1.                                                          TRYDCD 186
      IF ((UL .LE. SRHA) .OR. (BL .GE. SSHA))  GO TO 1044                       TRYDCD 187
      IF (RKTS(IH) .EQ. IUNDEF)  GO TO 1042                                     TRYDCD 188
C              VALUE THERE                                                      TRYDCD 189
      PRKTS  = RKTS(IH)                                                         TRYDCD 190
      IHRSP  = 0                                                                TRYDCD 191
      GO TO 1044                                                                TRYDCD 192
C              VALUE MISSING                                                    TRYDCD 193
 1042 IHRSP  = IHRSP + 1                                                        TRYDCD 194
      IHRNXT = 0                                                                TRYDCD 195
 1043 IHRNXT = IHRNXT + 1                                                       TRYDCD 196
      IHRX   = IHRNXT + IH                                                      TRYDCD 197
      BLNXT = FLOAT(IHRX) - 12. - 1.                                            TRYDCD 198
      IF (BLNXT .GE. SSHA)  RKTS(IHRX) = 0.                                     TRYDCD 199
      IF (RKTS(IHRX) .EQ. IUNDEF)  GO TO 1043                                   TRYDCD 200
      IF ((IHRNXT+IHRSP) .GT. INTINT)  WRITE (OUTFIL,9003) INTINT,              TRYDCD 201
     1   MNAMES(IMNTH), IDAY                                                    TRYDCD 202
      RKTS(IH) = PRKTS + (RKTS(IHRX)-PRKTS)*FLOAT(IHRSP)/                       TRYDCD 203
     1           (FLOAT(IHRNXT)+FLOAT(IHRSP))                                   TRYDCD 204
      IF ((SRHA .GT. BL) .OR. (SSHA .LT. UL))  GO TO 1044                       TRYDCD 205
      WRITE (OUTFIL,9002) IH, MNAMES(IMNTH), IDAY                               TRYDCD 206
 1044 CONTINUE                                                                  TRYDCD 207
 9002 FORMAT(1H ,15H--- WARNING ---, 25H SOLAR RADIATION MISSING ,              TRYDCD 208
     1       15HFOR SOLAR HOUR ,I2,4H ON ,A4,I2)                                TRYDCD 209
 9003 FORMAT(1H ,15H--- WARNING ---,27H INTERPOLATION INTERVAL OF ,             TRYDCD 210
     1       I2,39H HOURS EXCEEDED FOR SOLAR RADIATION ON ,A4,I2)               TRYDCD 211
      DO 1048 IH=1,24                                                           TRYDCD 212
C              PUT LOCAL TIME IN TERMS OF HOUR ANGLE                            TRYDCD 213
      UL     = FLOAT(IH) - 12. + FLOAT(NTZ) + DEABC(2) - XLONG/15.              TRYDCD 214
      BL     = UL - 1.                                                          TRYDCD 215
C              SET BIN EDGES OF OVERLAPPING SOLAR TIME BINS                     TRYDCD 216
      SB2    = FLOAT(INT(UL))                                                   TRYDCD 217
      IF (UL .LT. 0.0)  SB2 = SB2 - 1.                                          TRYDCD 218
      SB1    = SB2 - 1.                                                         TRYDCD 219
      SB3    = SB2 + 1.                                                         TRYDCD 220
      IF ((UL .LE. SRHA) .OR. (BL .GE. SSHA))  GO TO 1048                       TRYDCD 221
C              RESET BIN EDGES IF BINS OVERLAP SUNRISE OR SUNSET                TRYDCD 222
      IF (SRHA .GT. BL)  BL = SRHA                                              TRYDCD 223
      IF (SRHA .GT. SB1)  SB1 = SRHA                                            TRYDCD 224
      IF (SSHA .LT. UL)  UL = SSHA                                              TRYDCD 225
      IF (SSHA .LT. SB3)  SB3 = SSHA                                            TRYDCD 226
C              INTERPOLATE RKT                                                  TRYDCD 227
      ISB2   = INT(12.01+SB2)                                                   TRYDCD 228
      ISB3   = ISB2 + 1                                                         TRYDCD 229
      IF ((UL-BL) .LE. .02)  GO TO 1048                                         TRYDCD 230
      IF ((SB2-SB1) .LT. .02)  RKT = RKTS(ISB3)                                 TRYDCD 231
      IF ((SB3-SB2) .LT. .02)  RKT = RKTS(ISB2)                                 TRYDCD 232
      IF (((SB2-SB1) .GE. .02) .AND. ((SB3-SB2) .GE. .02))                      TRYDCD 233
     1   RKT = RKTS(ISB2)*(SB2-BL)/(UL-BL) + RKTS(ISB3)*(UL-SB2)                TRYDCD 234
     2   /(UL-BL)                                                               TRYDCD 235
      COSZIN = A*(UL-BL) + B*(SIN(PIOV12*UL)-SIN(PIOV12*BL))/PIOV12             TRYDCD 236
      COSZAV = COSZIN/(UL-BL)                                                   TRYDCD 237
      SOLH   = SOLCON*COSZIN                                                    TRYDCD 238
      SOLHM  = SOLH*RKT                                                         TRYDCD 239
      NEWSSM = NEWSSM + SOLHM                                                   TRYDCD 240
C              GET DIFFUSE COMPONENT FROM ERBS, KLEIN, AND DUFFIE               TRYDCD 241
C              CORRELATION                                                      TRYDCD 242
      IF (RKT .LE. 0.22)  DIF = SOLHM*(1.-0.09*RKT)                             TRYDCD 243
      RKT2   = RKT*RKT                                                          TRYDCD 244
      IF ((RKT .GT. 0.22) .AND. (RKT .LE. 0.8))  DIF = SOLHM*                   TRYDCD 245
     1   (.9511-.1604*RKT+4.388*RKT2-16.638*RKT*RKT2+12.336*RKT2*RKT2)          TRYDCD 246
      IF (RKT .GT. 0.8)  DIF = 0.165*SOLHM                                      TRYDCD 247
      DIRH   = AMAX1(0.,SOLHM-DIF)                                              TRYDCD 248
      DIRN(IH) = DIRH/COSZAV                                                    TRYDCD 249
      SOLHOR(IH) = SOLHM                                                        TRYDCD 250
C              CHECK FOR MAXIMUM DIRECT NORMAL                                  TRYDCD 251
      CALL MAXDIR(COSZAV,SOLCON,DIRMAX)                                         TRYDCD 252
      DIRN(IH) = AMIN1(DIRN(IH),DIRMAX)                                         TRYDCD 253
C              IF (IMNTH .NE. 5)  GO TO 1048                                    TRYDCD 254
C     PRINT 9001,IH,IDAY,MNAMES(IMNTH),DEABC(1),DEABC(2),RKTS(ISB3),            TRYDCD 255
C              +           SB2,                                                 TRYDCD 256
C    +           SSHA,SRHA,UL,BL,A,B,SOLCON,COSZIN,COSZAV,SOLH,                 TRYDCD 257
C    +           SOLHM,RKT,DIF,DIRH,DIRN(IH),ISB2,ISB3,RKTS(ISB2),              TRYDCD 258
C              +           DIRMAX                                               TRYDCD 259
 9001 FORMAT(1H ,9HIH     = ,I11,10X,9HIDAY   = ,I11,10X,9HMNAMES = ,           TRYDCD 260
     1      7X,A4,10X,9HDEABC1 = ,F11.5/1H ,9HDEABC2 = ,F11.5,10X,              TRYDCD 261
     2      9HRKTS3  = ,F11.5,10X,9HSB2    = ,F11.3,10X,9HSSHA   = ,            TRYDCD 262
     3      F11.3/1H ,9HSRHA   = ,F11.3,10X,9HUL     = ,F11.3,10X,              TRYDCD 263
     4      9HBL     = ,F11.3,10X,9HA      = ,F11.5/1H ,9HB      = ,            TRYDCD 264
     5      F11.5,10X,9HSOLCON = ,F11.3,10X,9HCOSZIN = ,F11.5,10X,              TRYDCD 265
     6      9HCOSZAV = ,F11.5/1H ,9HSOLH   = ,F11.3,10X,9HSOLHM  = ,            TRYDCD 266
     7      F11.3,10X,9HRKT    = ,F11.5,10X,9HDIF    = ,F11.3/1H ,              TRYDCD 267
     8      9HDIRH   = ,F11.3,10X,9HDIRN   = ,F11.3,10X,9HISB2   = ,            TRYDCD 268
     9      I11,10X,9HISB3   = ,I11/1H ,9HRKTS2  = ,F11.5,10X,                  TRYDCD 269
     1      9HDIRMAX = ,F11.3)                                                  TRYDCD 270
 1048 CONTINUE                                                                  TRYDCD 271
C              RENORMALIZE AND STORE SOLAR DATA                                 TRYDCD 272
      RATNRM = SUNSUM/NEWSSM                                                    TRYDCD 273
      DO 2048 IH=1,24                                                           TRYDCD 274
      DIRN(IH) = DIRN(IH)*RATNRM                                                TRYDCD 275
      SOLHOR(IH) = SOLHOR(IH)*RATNRM                                            TRYDCD 276
      IDN(IH) = IROUND(DIRN(IH))                                                TRYDCD 277
      ISOL(IH) = IROUND(SOLHOR(IH))                                             TRYDCD 278
 2048 CONTINUE                                                                  TRYDCD 279
 1049 CONTINUE                                                                  TRYDCD 280
C              SET CLOUD TYPE TO DOE-TYPE ^0-2[                                 TRYDCD 281
      DO 1050 IH = 1,24                                                         TRYDCD 282
      ICLTY1(IH) = ICLTY(IH)                                                    TRYDCD 283
      IF (ICLTY(IH) .EQ. IUNDEF)  GO TO 1050                                    TRYDCD 284
      IC     = 0                                                                TRYDCD 285
      IF (ICLTY(IH) .LT. 8) IC = 2                                              TRYDCD 286
      IF (ICLTY(IH) .EQ. 2) IC = 1                                              TRYDCD 287
      ICLTY(IH) = IC                                                            TRYDCD 288
 1050 CONTINUE                                                                  TRYDCD 289
C              SET WIND DIRECTION TO ^0-15[                                     TRYDCD 290
      DO 1060 IH = 1,24                                                         TRYDCD 291
      IF (IWNDSP(IH) .EQ. 0)  IWNDIR(IH) = 0                                    TRYDCD 292
      IF (IWNDSP(IH) .EQ. 0)  GO TO 1060                                        TRYDCD 293
      IF (IWNDIR(IH) .EQ. IUNDEF)  GO TO 1060                                   TRYDCD 294
      IWNDIR(IH) = INT(FLOAT(IWNDIR(IH))*4.444444E-2 + 0.5)                     TRYDCD 295
      IF (IWNDIR(IH) .EQ. 16)  IWNDIR(IH) = 0                                   TRYDCD 296
 1060 CONTINUE                                                                  TRYDCD 297
      IDAYL  = IDAY                                                             TRYDCD 298
      RETURN                                                                    TRYDCD 299
      END                                                                       TRYDCD 300
      SUBROUTINE VALIDA(IVAR,ICODE,NCDWDS,NAM,ERR,INDEX)                        VALIDA   2
C                                                                               VALIDA   3
C              ECHO PRINT AND VALIDATE ALPHA INPUT                              VALIDA   4
C                                                                               VALIDA   5
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      DIMENSION IVAR(2), NAM(4), ICODE(2,NCDWDS)                                VALIDA   7
      LOGICAL ERR                                                               VALIDA   8
      DATA IBLNK /4H    /                                                       VALIDA   9
C              ECHO OUT VARIABLE                                                VALIDA  10
      WRITE(OUTFIL,901) NAM, IVAR                                               VALIDA  11
C              CHECK FOR IVAR IN ICODE ARRAY                                    VALIDA  12
      IF (IVAR(1) .EQ. IBLNK)  INDEX = 1                                        VALIDA  13
      IF (IVAR(1) .EQ. IBLNK)  GO TO 100                                        VALIDA  14
      INDEX  = 0                                                                VALIDA  15
      DO 10 I=1,NCDWDS                                                          VALIDA  16
      IF ((IVAR(1) .EQ. ICODE(1,I)) .AND. (IVAR(2) .EQ. ICODE(2,I)))            VALIDA  17
     1  INDEX = I                                                               VALIDA  18
   10 CONTINUE                                                                  VALIDA  19
      IF (INDEX .NE. 0)  GO TO 100                                              VALIDA  20
      ERR    = .TRUE.                                                           VALIDA  21
      WRITE(OUTFIL,902) IVAR, NAM                                               VALIDA  22
  100 CONTINUE                                                                  VALIDA  23
      RETURN                                                                    VALIDA  24
  901 FORMAT(11X,4A4,8X,A4,A2)                                                  VALIDA  25
  902 FORMAT(1H ,14H--- ERROR --- ,A4,A2,26H IS NOT A VALID INPUT FOR ,         VALIDA  26
     1       4A4,33H.  PROCESSING WILL BE TERMINATED.)                          VALIDA  27
      END                                                                       VALIDA  28
      SUBROUTINE VALIDF(VAR,VMIN,VMAX,NAM,NCNT,ERR)                             VALIDF   2
C                                                                               VALIDF   3
C              ECHO PRINT AND VALIDATE FLOATING POINT INPUT                     VALIDF   4
C                                                                               VALIDF   5
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
      DIMENSION NAM(4)                                                          VALIDF   8
      LOGICAL ERR                                                               VALIDF   9
C              ECHO OUT VARIABLE                                                VALIDF  10
      IF (VAR .EQ. UNDEF)  GO TO 100                                            VALIDF  11
      IF (NCNT .LE. 0)  WRITE(OUTFIL,901) NAM, VAR                              VALIDF  12
      IF (NCNT .GT. 0)  WRITE(OUTFIL,902) NAM, NCNT, VAR                        VALIDF  13
C              CHECK FOR LIMITS ON VAR                                          VALIDF  14
      IF (VAR .GE. VMIN .AND. VAR .LE. VMAX)  GO TO 100                         VALIDF  15
      WRITE(OUTFIL,903) NAM, VMIN, VMAX                                         VALIDF  16
      ERR    = .TRUE.                                                           VALIDF  17
  100 CONTINUE                                                                  VALIDF  18
      RETURN                                                                    VALIDF  19
  901 FORMAT(11X,4A4,6X,F8.3)                                                   VALIDF  20
  902 FORMAT(11X,4A4,1X,1H(,I2,1H),1X,F8.3)                                     VALIDF  21
  903 FORMAT(1H ,14H--- ERROR --- ,4A4,26H IS OUTSIDE ALLOWED RANGE.,           VALIDF  22
     1       23H  THE ALLOWED RANGE IS ,F6.1,4H TO ,F6.1,                       VALIDF  23
     2       33H.  PROCESSING WILL BE TERMINATED.)                              VALIDF  24
      END                                                                       VALIDF  25
      SUBROUTINE VALIDI(IVAR,MIN,MAX,NAM,NCNT,ERR)                              VALIDI   2
C                                                                               VALIDI   3
C              ECHO PRINT AND VALIDATE INTEGER DATA                             VALIDI   4
C                                                                               VALIDI   5
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
      DIMENSION NAM(4)                                                          VALIDI   8
      LOGICAL ERR                                                               VALIDI   9
C              ECHO OUT IVAR                                                    VALIDI  10
      IF (IVAR .EQ. IUNDEF)  GO TO 100                                          VALIDI  11
      IF (NCNT .LE. 0)  WRITE(OUTFIL,901) NAM, IVAR                             VALIDI  12
      IF (NCNT .GT. 0)  WRITE(OUTFIL,902) NAM, NCNT, IVAR                       VALIDI  13
C              CHECK FOR MIN AND MAX                                            VALIDI  14
      IF (IVAR .GE. MIN .AND. IVAR .LE. MAX)  GO TO 100                         VALIDI  15
      WRITE(OUTFIL,903) NAM, MIN, MAX                                           VALIDI  16
      ERR    = .TRUE.                                                           VALIDI  17
  100 CONTINUE                                                                  VALIDI  18
      RETURN                                                                    VALIDI  19
  901 FORMAT(11X,4A4,6X,I8)                                                     VALIDI  20
  902 FORMAT(11X,4A4,1X,1H(,I2,1H),1X,I8)                                       VALIDI  21
  903 FORMAT(1H ,14H--- ERROR --- ,4A4,26H IS OUTSIDE ALLOWED RANGE.,           VALIDI  22
     1       23H  THE ALLOWED RANGE IS ,I6,4H TO ,I6,                           VALIDI  23
     2       33H.  PROCESSING WILL BE TERMINATED.)                              VALIDI  24
      END                                                                       VALIDI  25
      SUBROUTINE WBDP(DBF,RHF,PHG,WETBF,DEWPF)                                  WBDP     2
C                                                                               WBDP     3
C              CALCULATE WET BULB AND DEW POINT TEMPERATURE GIVEN THE           WBDP     4
C              DRY BULB, RELATIVE HUMIDITY, AND PRESSURE                        WBDP     5
C                                                                               WBDP     6
C              INPUTS                                                           WBDP     7
C              ------                                                           WBDP     8
C                                                                               WBDP     9
C              DBF    - DRY BULB TEMPERATURE (DEG F)                            WBDP    10
C              RHF    - RELATIVE HUMIDITY (0 - 1)                               WBDP    11
C              PHG    - ATMOSPHERIC PRESSURE (INCHES OF HG)                     WBDP    12
C                                                                               WBDP    13
C              OUTPUTS                                                          WBDP    14
C              -------                                                          WBDP    15
C                                                                               WBDP    16
C              WETBF  - WET BULB TEMPERATURE (DEG F)                            WBDP    17
C              DEWPF  - DEW POINT TEMPERATURE (DEG F)                           WBDP    18
C                                                                               WBDP    19
C              REAL DBF,RHF,PHG,WETBF,DEWPF                                     WBDP    20
C                                                                               WBDP    21
C              SATURATED VAPOR PRESSURE                                         WBDP    22
      PS = PPWVMS(DBF)                                                          WBDP    23
C              PARTIAL PRESSURE                                                 WBDP    24
      PW = RHF*PS                                                               WBDP    25
C              HUMIDITY RATIO                                                   WBDP    26
      HR = 0.622*PW/(PHG-PW)                                                    WBDP    27
C              SPECIFIC ENTHALPY                                                WBDP    28
      ENTH = 0.24*DBF + (1061.+0.444*DBF)*HR                                    WBDP    29
      WETBF = WBF(ENTH,PHG)                                                     WBDP    30
      Y = LOG(PW)                                                               WBDP    31
      IF (PW.LE.0.1836) THEN                                                    WBDP    32
          DEWPF = 71.98 + 24.873*Y + 0.8927*Y*Y                                 WBDP    33
      ELSE                                                                      WBDP    34
          DEWPF = 79.047 + 30.579*Y + 1.8893*Y*Y                                WBDP    35
      END IF                                                                    WBDP    36
      RETURN                                                                    WBDP    37
      END                                                                       WBDP    38
      FUNCTION WBF(H, PB)                                                       WBF      2
C                                                                               WBF      3
C              FINDS WET BULB TEMPERATURE WHEN ENTHALPY                         WBF      4
C              AND PRESSURE ARE GIVEN                                           WBF      5
C                                                                               WBF      6
      IF ((PB .LT. 29.3) .OR. (PB .GT. 30.5))  GO TO 2                          WBF      7
      IF (H .LE. 2.0)  GO TO 2                                                  WTH2    71
C              IF WITHIN 2 PER CENT OF STANDARD PRESSURE AND ENTHALPY           WBF      9
C              IS POSITIVE, DO SIMPLE CURVE-FIT CALCULATION                     WBF     10
      Y      = ALOG(H)                                                          WBF     11
      IF (H.GT.11.758) GO TO 3                                                  WBF     12
      WBF    = 0.6041+3.4841*Y+1.3601*Y*Y+0.97307*Y*Y*Y                         WBF     13
      GO TO 4                                                                   WBF     14
    3 WBF    = 30.9185-39.68200*Y+20.5841*Y*Y-1.758*Y*Y*Y                       WBF     15
      GO TO 4                                                                   WBF     16
C              OTHERWISE ITERATE                                                WBF     17
    2 WB1    = 150.                                                             WBF     18
      PV1 = PPWVMS(WB1)                                                         WBF     19
      W1     = 0.622*PV1/(PB-PV1)                                               WBF     20
      X1     = 0.24*WB1+(1061.+0.444*WB1)*W1                                    WBF     21
      Y1     = H-X1                                                             WBF     22
    9 WB2    = WB1-1.                                                           WBF     23
      PV2 = PPWVMS(WB2)                                                         WBF     24
      W2     = 0.622*PV2/(PB-PV2)                                               WBF     25
      X2     = 0.24*WB2+(1061.+0.444*WB2)*W2                                    WBF     26
      Y2     = H-X2                                                             WBF     27
      IF (Y1*Y2) 6,7,8                                                          WBF     28
    8 WB1    = WB2                                                              WBF     29
      Y1     = Y2                                                               WBF     30
      GO TO 9                                                                   WBF     31
    7 IF (Y1) 10,11,10                                                          WBF     32
   11 WBF    = WB1                                                              WBF     33
      GO TO 4                                                                   WBF     34
   10 WBF    = WB2                                                              WBF     35
      GO TO 4                                                                   WBF     36
    6 Z      = ABS(Y1/Y2)                                                       WBF     37
      WBF    = (WB2*Z+WB1)/(1.+Z)                                               WBF     38
    4 RETURN                                                                    WBF     39
      END                                                                       WBF     40
      SUBROUTINE WDREAD                                                         WDREAD   2
C                                                                               WDREAD   3
C              THIS SUBROUTINE UNPACKS ONE HOUR OF A PACKED DOE-N               WDREAD   4
C              WEATHER FILE (TWO 60 BIT OR FOUR 30 BIT WORDS PER HOUR).         WDREAD   5
C              INFO IS STORED IN ARRAY CALC, WHICH IS EQUIVALENCED TO           WDREAD   6
C              THE FOLLOWING LIST ---                                           WDREAD   7
C              WET BULB TEMP          DEG F    HUMIDITY RATIO                   WDREAD   8
C              DRY BULB TEMP          DEG F    DENSITY          LBM/FT3         WDREAD   9
C              ATMOSPHERIC PRESSURE   IN HG    ENTHALPY         BTU/LBM         WDREAD  10
C              CLOUD AMOUNT           TENTHS   SOLAR RADIATION  BTU/FT2         WDREAD  11
C              SNOW FLAG(UNUSED)      0 OR 1   DIRECT SOLAR     BTU/FT2         WDREAD  12
C              RAIN FLAG(UNUSED)      0 OR 1   CLOUD TYPE       CODE 1-9        WDREAD  13
C              WIND DIRECTION     0 - 15 PTS   WIND SPEED         KNOTS         WDREAD  14
C                                                                               WDREAD  15
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /FILTYP/   IFTYP,IWSZ,ITPTYP                                      /FILTYP/ 2
C                                                                               /FILTYP/ 3
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /IDAT/     IDAT(1536)                                             /IDAT/   2
C                                                                               /IDAT/   3
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /WEATHD/   IWDID(5),FIRSTW,LRECX,IWSOL,WLONG,WLAT,IWTZN,          /WEATHD/ 2
     1                   CLRNES,TGRND,IWYR,WBT,DBT,PATM,CLDAMT,ISNOW,           /WEATHD/ 3
     2                   IRAIN,IWNDDR,HUMRAT,DENSTY,ENTHAL,SOLRAD,              /WEATHD/ 4
     3                   DIRSOL,ICLDTY,WNDSPD,IRFCP,DPT                         /WEATHD/ 5
      LOGICAL FIRSTW                                                            /WEATHD/ 6
      DIMENSION          CALC(16)                                               /WEATHD/ 7
      EQUIVALENCE        (WBT,CALC(1))                                          /WEATHD/ 8
C                                                                               /WEATHD/ 9
C                                                                               WDREAD  22
      DIMENSION  XMASK(16,2)                                                    WDREAD  24
      DIMENSION LOOK(14), IWDIDO(2)                                             WDREAD  25
      DIMENSION IDAT30(1536), IDAT60(768), IDATO(1488)                          WDREAD  26
C                                                                               WDREAD  27
      EQUIVALENCE (IDAT(1),IDAT30(1)),(IDAT(1),IDAT60(1)),                      WDREAD  28
     1            (IDAT(1),IDATO(1))                                            WDREAD  29
C                                                                               WDREAD  30
C                                                                               WDREAD  31
      DATA  XMASK    /-99.0,-99.0,15.0,0.0,0.0,0.0,0.0,0.0,0.02,-30.0,          WDREAD  36
     1                0.0,0.0,0.0,0.0,0.0,10.0,1.0,1.0,0.1,1.0,1.0,1.0,         WDREAD  37
     2                1.0,0.0001,0.001,0.5,1.0,1.0,1.0,1.0,0.0,0.0              WDREAD  38
     3               /                                                          WDREAD  39
C                                                                               WDREAD  40
C              LRECX TELLS HOW MANY RECORDS HAVE BEEN READ                      WDREAD  41
      IF (.NOT. FIRSTW)  GO TO 20                                               WDREAD  42
      FIRSTW = .FALSE.                                                          WDREAD  43
      READ(INWTH,END=295) LOOK                                                  WDREAD  44
      IWSOL  = LOOK(14)                                                         WDREAD  45
      IF (LOOK(11) .EQ. 0) IWSOL = 0                                            WDREAD  46
      IWSIZ  = MOD(IWSOL,2) + 1                                                 WDREAD  47
      IFX    = IWSOL + 1                                                        WDREAD  48
      BACKSPACE INWTH                                                           WDREAD  49
   20 CONTINUE                                                                  WDREAD  50
      IRECX  = IMNTH                                                            WDREAD  51
      IDX    = IDAY                                                             WDREAD  52
      IF (IFX .LT. 3)  GO TO 30                                                 WDREAD  53
      IRECX  = IMNTH*2 + (IDAY-1)/16 - 1                                        WDREAD  54
      IDX    = MOD(IDAY-1,16) + 1                                               WDREAD  55
   30 CONTINUE                                                                  WDREAD  56
      IF (IRECX-LRECX) 100,300,200                                              WDREAD  57
  100 CONTINUE                                                                  WDREAD  58
C              BACKSPACE TO PROPER MONTH                                        WDREAD  59
      IDIF   = LRECX - IRECX + 1                                                WDREAD  60
      DO 120 I=1,IDIF                                                           WDREAD  61
      BACKSPACE INWTH                                                           WDREAD  62
  120 CONTINUE                                                                  WDREAD  63
  200 GO TO  (210,210,220,230,220,230), IFX                                     WDREAD  64
  210 READ (INWTH,END=295) IWDIDO,IWYR,WLAT,WLONG,IWTZN,LRECX,NUMDAY,           WDREAD  65
     .                     CLRNES,TGRND,IDUM,IDATO                              WDREAD  66
      GO TO 20                                                                  WDREAD  67
  220 READ (INWTH,END=295) IWDID,IWYR,WLAT,WLONG,IWTZN,LRECX,NUMDAY,            WDREAD  68
     .                     CLRNES,TGRND,IDUM,IDAT60                             WDREAD  69
      GO TO 20                                                                  WDREAD  70
  230 READ (INWTH,END=295) IWDID,IWYR,WLAT,WLONG,IWTZN,LRECX,NUMDAY,            WDREAD  71
     .                     CLRNES,TGRND,IDUM,IDAT30                             WDREAD  72
      GO TO 20                                                                  WDREAD  73
  295 WRITE(OUTFIL,9001)                                                        WDREAD  74
 9001 FORMAT(1H1////27H *** WEATHER TAPE ERROR ***)                             WDREAD  75
      IEOF   = .TRUE.                                                           WDREAD  76
      RETURN                                                                    WDREAD  77
  300 CONTINUE                                                                  WDREAD  78
      IF (IWSIZ .EQ. 2)  GO TO 700                                              WDREAD  79
  600 CONTINUE                                                                  WDREAD  80
      GO TO 2000                                                                WDREAD  81
  700 CONTINUE                                                                  WDREAD  82
      IP1    = 96*(IDX-1) + 4*IHOUR - 3                                         WDREAD  83
      LOOK(3) = IDAT(IP1)/65536                                                 WDREAD  84
      LOOK(1) = MOD(IDAT(IP1),65536)/256                                        WDREAD  85
      LOOK(2) = MOD(IDAT(IP1),256)                                              WDREAD  86
      LOOK(11) = IDAT(IP1+1)/1048576                                            WDREAD  87
      LOOK(12) = MOD(IDAT(IP1+1),1048576)/1024                                  WDREAD  88
      LOOK(4) = MOD(IDAT(IP1+1),1024)/64                                        WDREAD  89
      LOOK(5) = MOD(IDAT(IP1+1),64)/32                                          WDREAD  90
      LOOK(6) = MOD(IDAT(IP1+1),32)/16                                          WDREAD  91
      LOOK(7) = MOD(IDAT(IP1+1),16)                                             WDREAD  92
      LOOK(8) = IDAT(IP1+2)/128                                                 WDREAD  93
      LOOK(9) = MOD(IDAT(IP1+2),128)                                            WDREAD  94
      LOOK(10) = IDAT(IP1+3)/2048                                               WDREAD  95
      LOOK(13) = MOD(IDAT(IP1+3),2048)/128                                      WDREAD  96
      LOOK(14) = MOD(IDAT(IP1+3),128)                                           WDREAD  97
      DO 720 I=1,14                                                             WDREAD  98
      CALC(I) = FLOAT(LOOK(I))*XMASK(I,2) + XMASK(I,1)                          WDREAD  99
  720 CONTINUE                                                                  WDREAD 100
 2000 ISNOW  = INT(CALC(5))                                                     WDREAD 101
      IRAIN  = INT(CALC(6))                                                     WDREAD 102
      IWNDDR = INT(CALC(7))                                                     WDREAD 103
      ICLDTY = INT(CALC(13))                                                    WDREAD 104
      IF (IFTYP .EQ. 1)  WNDSPD=0.869*WNDSPD                                    WDREAD 105
      RETURN                                                                    WDREAD 106
      END                                                                       WDREAD 107
      SUBROUTINE WDTSNR                                                         WDTSNR   2
C                                                                               WDTSNR   3
C              CALCULATE WEATHER AND SUN HOURLY DATA                            WDTSNR   4
C                                                                               WDTSNR   5
C              THIS ROUTINE CORRESPONDS TO  W D T S U N  IN THE LOADS           WDTSNR   6
C                 PROGRAM. IT WAS REVISED TO ENABLE CALCULATION OF              WDTSNR   7
C                 ADDITIONAL SOLAR RADIATION VARIABLES FOR THE WEATHER          WDTSNR   8
C                 STATISTICS PORTION OF THE PROCESSOR.                          WDTSNR   9
C                 --VARIABLE CHANGES--                                          WDTSNR  10
C                   IWSOL  WAS  IFX                                             WDTSNR  11
C                   IWTZN  WAS  ITIMZ                                           WDTSNR  12
C                   IHOUR  WAS  IHR                                             WDTSNR  13
C                                                                               WDTSNR  14
      COMMON  /LOCALD/   STALAT,STALON,SSTALA,CSTALA,TSTALA,HRSLON              /LOCALD/ 2
C                                                                               /LOCALD/ 3
      COMMON /SUND/ISUNUP,GUNDOG,HORANG,TDECLN,EQTIME,SOLCON,                   /SUND/   2
     1        ATMEXT,SKYDFF,RAYCOS(3),RDN,BSUN,DECLN,CD,SD,                     /SUND/   3
     2        FSUNUP,CLDCOV,RDNCC,BSCC,DIFSOL,RTOT(4),SKYA                      /SUND/   4
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON /TIM/ IDOY                                                         /TIME/   2
      COMMON  /WEATHD/   IWDID(5),FIRSTW,LRECX,IWSOL,WLONG,WLAT,IWTZN,          /WEATHD/ 2
     1                   CLRNES,TGRND,IWYR,WBT,DBT,PATM,CLDAMT,ISNOW,           /WEATHD/ 3
     2                   IRAIN,IWNDDR,HUMRAT,DENSTY,ENTHAL,SOLRAD,              /WEATHD/ 4
     3                   DIRSOL,ICLDTY,WNDSPD,IRFCP,DPT                         /WEATHD/ 5
      LOGICAL FIRSTW                                                            /WEATHD/ 6
      DIMENSION          CALC(16)                                               /WEATHD/ 7
      EQUIVALENCE        (WBT,CALC(1))                                          /WEATHD/ 8
C                                                                               /WEATHD/ 9
C                                                                               WDTSNR  20
      DATA SBAZIM,CBAZIM/0.0,1.0/                                               WDTSNR  21
C              SET RADIATION LOSS TO SKY FOR                                    WDTSNR  22
C              TILT = 0                                                         WDTSNR  23
      SKYA   = ( 10. - CLDAMT ) * 2.                                            WDTSNR  24
C              CALCULATE HOUR ANGLE                                             WDTSNR  25
      HORANG = .2618 * (FLOAT(IHOUR-12+IWTZN) + EQTIME - 0.5) - STALON          WDTSNR  26
C              SET TEST TO BE THE HOUR ANGLE OF THE BIN EDGE                    WDTSNR  27
C              NEAREST NOON.                                                    WDTSNR  28
      TEST   = HORANG + .1309                                                   WDTSNR  29
      IF (IHOUR .GE. 12)  TEST = HORANG - .1309                                 WDTSNR  30
C              IF SUN IS DOWN, SKIP                                             WDTSNR  31
      IF ( ABS(TEST) .GT. ABS(GUNDOG) )  GO TO 1800                             WDTSNR  32
C              SUN IS UP.  SET FLAG                                             WDTSNR  33
      ISUNUP = 1                                                                WDTSNR  34
C              TEST TO SEE IF THIS HOUR BIN CONTAINS SUNRISE                    WDTSNR  35
C              OR SUNSET                                                        WDTSNR  36
      FSUNUP = 1.                                                               WDTSNR  37
      DIFF   = ABS(GUNDOG) - ABS(TEST)                                          WDTSNR  38
      IF ((DIFF .LT. 0.) .OR. (DIFF .GE. 0.2618))  GO TO 200                    WDTSNR  39
C              RESET THE HOUR ANGLE HALF WAY BETWEEN SUNRISE OR                 WDTSNR  40
C              SUNSET AND THE BIN EDGE NEAREST NOON.                            WDTSNR  41
      IF (IHOUR .LT. 12)  HORANG = HORANG + 0.5*(.2618-DIFF)                    WDTSNR  42
      IF (IHOUR .GE. 12)  HORANG = HORANG - 0.5*(.2618-DIFF)                    WDTSNR  43
C              SET FSUNUP TO BE THE FRACTION OF THE HOUR                        WDTSNR  44
C              THE SUN WAS UP                                                   WDTSNR  45
      FSUNUP = 3.8197*DIFF                                                      WDTSNR  46
  200 CONTINUE                                                                  WDTSNR  47
      CHCD   = COS( HORANG ) * CD                                               WDTSNR  48
      SHCD   = SIN( HORANG ) * CD                                               WDTSNR  49
      CLSD   = SD * CSTALA                                                      WDTSNR  50
      CHCDSC = CHCD*SSTALA - CLSD                                               WDTSNR  51
C              CALCULATE SOLAR DIRECTION COSINE                                 WDTSNR  52
      RAYCOS(1) = CHCDSC*SBAZIM - SHCD*CBAZIM                                   WDTSNR  53
      RAYCOS(2) = -CHCDSC*CBAZIM - SHCD*SBAZIM                                  WDTSNR  54
      RAYCOS(3) = SSTALA*SD + CSTALA*CHCD                                       WDTSNR  55
      IF (RAYCOS(3) .LT. 0.001)  GO TO 1800                                     WDTSNR  56
      IF (IWSOL .GE. 4) GO TO 2000                                              WDTSNR  57
C              CALCULATE DIRECT NORMAL AND DIFFUSE RADIATION                    WDTSNR  58
C              FOR SKY WITH NO CLOUDS                                           WDTSNR  59
C              IBM MACHINE PROBLEM WITH EXPONENT UNDERFLOW FOR EXP( ).          WDTSNR  60
C     PROBABLE CAUSE< RAYCOS(3) VERY SMALL --> ARGUMENT VERY LARGE.             WDTSNR  61
C     FIX<  CHECK ARGUMENT FOR LARGE NEGATIVE NUMBER. RDN=0 IF SO.              WDTSNR  62
      ARGUE  = -ATMEXT / RAYCOS(3)                                              WDTSNR  63
      IF(ARGUE .LT. -50.0)  GOTO 1                                              WDTSNR  64
      RDN    = SOLCON * CLRNES * EXP( ARGUE )                                   WDTSNR  65
      GO TO 2                                                                   WDTSNR  66
C              1    WRITE (6,3)                                                 WDTSNR  67
C3    FORMAT (' >>>>NOTE< WILL HAVE EXPONENT UNDERFLOW.  RESULT 0')             WDTSNR  68
C              LET'S NOT PRINT OUT ANY MESSAGES (JUST A NUSIANCE <)             WDTSNR  69
    1 CONTINUE                                                                  WDTSNR  70
      RDN    = 0.0                                                              WDTSNR  71
    2 CONTINUE                                                                  WDTSNR  72
      BSUN   = RDN * SKYDFF / ( CLRNES*CLRNES )                                 WDTSNR  73
C              GET CLDCOV, THE CLOUD COVER MODIFIER                             WDTSNR  74
      CALL  CCM                                                                 WDTSNR  75
      IF (CLDAMT .LE. 0.001) CLDCOV = 1.0                                       WDTSNR  76
C              GET DIRECT NORMAL AND DIFFUSE FOR                                WDTSNR  77
C              A CLOUDY SKY                                                     WDTSNR  78
      SOLRAD = (RDN*RAYCOS(3)+BSUN)*CLDCOV*FSUNUP                               WDTSNR  79
      RDNCC  = FSUNUP*RDN*(1.-0.1*CLDAMT)                                       WDTSNR  80
      BSCC   = SOLRAD - RDNCC*RAYCOS(3)                                         WDTSNR  81
      BSCC   = AMAX1(BSCC,0.)                                                   WDTSNR  82
      DIRSOL = RDNCC                                                            WDTSNR  83
      SOLRAD = BSCC+DIRSOL*RAYCOS(3)                                            WDTSNR  84
      GO TO 3000                                                                WDTSNR  85
C              SUN DOWN                                                         WDTSNR  86
 1800 ISUNUP = 0                                                                WDTSNR  87
      RDN    = 0.                                                               WDTSNR  88
      BSUN   = 0.                                                               WDTSNR  89
      CLDCOV = 0.                                                               WDTSNR  90
      RDNCC  = 0.                                                               WDTSNR  91
      BSCC   = 0.                                                               WDTSNR  92
      SOLRAD = 0.                                                               WDTSNR  93
      DIRSOL = 0.                                                               WDTSNR  94
      DIFSOL = 0.                                                               WDTSNR  95
      RAYCOS(3) = 0.                                                            WDTSNR  96
      GO TO 3000                                                                WDTSNR  97
 2000 CONTINUE                                                                  WDTSNR  98
C              USE MEASURED SOLAR DATA                                          WDTSNR  99
      RDN    = 0.                                                               WDTSNR 100
      BSUN   = 0.                                                               WDTSNR 101
      RDNCC  = DIRSOL                                                           WDTSNR 102
      BSCC   = SOLRAD - DIRSOL*RAYCOS(3)                                        WDTSNR 103
      IF (IWSOL.GE.6)  BSCC=DIFSOL                                              WDTSNR 104
      IF (BSCC .LT. 0.)  BSCC = 0.                                              WDTSNR 105
 3000 CONTINUE                                                                  WDTSNR 106
      RETURN                                                                    WDTSNR 107
      END                                                                       WDTSNR 108
      SUBROUTINE WINDT(NB,NHRWIN,IT99,IT975)                                    WINDT    2
      COMMON  /MONTHC/   BEFORE(12),MDAYS(12),MNAMES(12)                        /MONTHC/ 2
      INTEGER            BEFORE                                                 /MONTHC/ 3
C                                                                               /MONTHC/ 4
C                                                                               WINDT    4
      DIMENSION  NB(250)                                                        WINDT    5
C                                                                               WINDT    6
      NSUM   = 0                                                                WINDT    7
      IT975  = 0                                                                WINDT    8
      IT99   = 0                                                                WINDT    9
      DO 1000 I = 1,200                                                         WINDT   10
      NSUM   = NSUM + NB(I)                                                     WINDT   11
      ITEMP  = I - 99                                                           WINDT   12
      PERC   = 100.0*FLOAT(NSUM)/FLOAT(NHRWIN)                                  WINDT   13
      IF(PERC .LT. 1.0) IT99 = ITEMP                                            WINDT   14
      IF(PERC .GE. 2.5) GOTO 3000                                               WINDT   15
      IT975  = ITEMP                                                            WINDT   16
 1000 CONTINUE                                                                  WINDT   17
 3000 CONTINUE                                                                  WINDT   18
      RETURN                                                                    WINDT   19
      END                                                                       WINDT   20
      SUBROUTINE WPACK                                                          WPACK    2
C                                                                               WPACK    3
C              PACK WEATHER QUANTITIES INTO IDAT                                WPACK    4
C                                                                               WPACK    5
      COMMON  /FILTYP/   IFTYP,IWSZ,ITPTYP                                      /FILTYP/ 2
C                                                                               /FILTYP/ 3
      COMMON  /IDAT/     IDAT(1536)                                             /IDAT/   2
C                                                                               /IDAT/   3
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /WEATHD/   IWDID(5),FIRSTW,LRECX,IWSOL,WLONG,WLAT,IWTZN,          /WEATHD/ 2
     1                   CLRNES,TGRND,IWYR,WBT,DBT,PATM,CLDAMT,ISNOW,           /WEATHD/ 3
     2                   IRAIN,IWNDDR,HUMRAT,DENSTY,ENTHAL,SOLRAD,              /WEATHD/ 4
     3                   DIRSOL,ICLDTY,WNDSPD,IRFCP,DPT                         /WEATHD/ 5
      LOGICAL FIRSTW                                                            /WEATHD/ 6
      DIMENSION          CALC(16)                                               /WEATHD/ 7
      EQUIVALENCE        (WBT,CALC(1))                                          /WEATHD/ 8
C                                                                               /WEATHD/ 9
C                                                                               WPACK   10
C              SET UP QUANTITIES TO BE PACKED                                   WPACK   11
      ISOL   = INT(SOLRAD + .5)                                                 WPACK   12
      IDN    = INT(DIRSOL + .5)                                                 WPACK   13
      IWET   = INT(WBT+99.5)                                                    WPACK   14
      IDRY   = INT(DBT+99.5)                                                    WPACK   15
      IPRES  = INT(PATM*10.-149.5)                                              WPACK   16
      ICLDAM = INT(CLDAMT)                                                      WPACK   17
      IWNDSP = INT(WNDSPD+0.5)                                                  WPACK   18
      IHUMRT = INT(HUMRAT*10000.+0.5)                                           WPACK   19
      IDENS  = INT(DENSTY*1000.-19.5)                                           WPACK   20
      IENTH  = INT(ENTHAL*2.0+60.5)                                             WPACK   21
      IF (IWSZ .EQ. 2)  GO TO 20                                                WPACK   22
C              PACK INTO 60 BIT WORD                                            WPACK   23
   10 CONTINUE                                                                  WPACK   24
C              PACK INTO 30 BIT WORD                                            WPACK   25
   20 IP1    = (IDXO-1)*96 + IHOUR*4 - 3                                        WPACK   26
      IDAT(IP1) = IPRES*65536 + IWET*256 + IDRY                                 WPACK   27
      IDAT(IP1+1) = ISOL*1048576 + IDN*1024 + ICLDAM*64                         WPACK   28
     1              + ISNOW*32 + IRAIN*16 + IWNDDR                              WPACK   29
      IDAT(IP1+2) = IHUMRT*128 + IDENS                                          WPACK   30
      IDAT(IP1+3) = IENTH*2048 + ICLDTY*128 + IWNDSP                            WPACK   31
      RETURN                                                                    WPACK   32
      END                                                                       WPACK   33
      SUBROUTINE WXDCD                                                          WXDCD    2
C                                                                               WXDCD    3
C              THIS ROUTINE DECODES A DAY OF WX1440 DATA STARTING WITH          WXDCD    4
C              THE FIRST CARD OF THE YEAR 'IYR'                                 WXDCD    5
C                                                                               WXDCD    6
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /FILTYP/   IFTYP,IWSZ,ITPTYP                                      /FILTYP/ 2
C                                                                               /FILTYP/ 3
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /PACKEC/   NTZ,ISTAT,XLAT,XLONG,                                  /PACKEC/ 2
     1                   IYR,INTINT,DDBT,IBEGH,NBS,IDOYSH(5),ISSHFT(5),         /PACKEC/ 3
     2                   ISHFT,ALT,SKYCOV(12),ILOC                              073a     1
C                                                                               /PACKEC/ 5
      COMMON  /PARAMS/   STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 2
      LOGICAL            STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 3
C                                                                               /PARAMS/ 4
      COMMON  /PCKINT/   KYR,KSTAT,CN(12),TG(12),KTIM,KARD(500),KYRL            -089     1
C                                                                               /PCKINT/ 3
      COMMON  /RAWDAT/   IDRY(24),IWET(24),IDEW(24),IPRESS(24),                 /RAWDAT/ 2
     1                   IWNDSP(24),ICLAMT(24),ISOL(24),IDN(24),                /RAWDAT/ 3
     2                   IWNDIR(24),ICLTY(24),IRN(24),ISN(24),                  CD144S   1
     3                   IOPQCA(24),IRELH(24),ICLTY1(24)                        CD144S   2
      DIMENSION IRAW(24,14)                                                     CD144S   3
      EQUIVALENCE        (IDRY(1),IRAW(1,1))                                    /RAWDAT/ 6
      COMMON /LSTIME/   LSTHRS(24)                                              /RAWDAT/ 7
C                                                                               /RAWDAT/ 8
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
C                                                                               WXDCD   16
      DIMENSION  IRANGE(2,8)                                                    WXDCD   17
      DIMENSION  ITEMP(8)                                                       WXDCD   18
      DIMENSION  NWNDIR(16)                                                     WXDCD   20
C                                                                               WXDCD   21
      DATA  IDASH    /1H-/                                                      WXDCD   22
      DATA  IRANGE   /16,18,   19,21,   22,24,   34,37,                         WXDCD   23
     1                13,15,   43,43,   11,12,   46,46/                         WXDCD   24
      DATA  NWNDIR   /11,12,22,32,33,34,44,54,55,56,66,76,77,78,88,18/          WXDCD   25
C                                                                               WXDCD   26
C                                                                               WXDCD   27
C              LOOP THROUGH UNTIL FOUND RIGHT YEAR                              WXDCD   28
      IF (IDAY .EQ. IDAYL)  RETURN                                              WXDCD   29
      IF (.NOT. FIRST) GOTO 3000                                                WXDCD   30
      CALL WXSTRT                                                               073b    85
      IF (IEOF) GOTO 3070                                                       WXDCD   32
 3000 CONTINUE                                                                  WXDCD   33
C              INITIALIZE RAIN AND SNOW FLAGS                                   WXDCD   34
      DO 1010 IH = 1,24                                                         WXDCD   35
      IRN(IH) = 0                                                               WXDCD   36
      ISN(IH) = 0                                                               WXDCD   37
 1010 CONTINUE                                                                  WXDCD   38
C              FILL IN VALUES FOR EACH HOUR                                     WXDCD   39
      DO 1040 IH = 1,24                                                         WXDCD   40
      ISTART = (MOD(IH-1,6))*80 + 15                                            WXDCD   41
      IF (ISTART .NE. 15 .OR. FIRST) GOTO 3010                                  WXDCD   42
      CALL GETCRD(INWTH,KARD,495)                                               WXDCD   43
      IF (IEOF) GOTO 3070                                                       WXDCD   44
C              IBM********** REPLACE THE ABOVE 2 CARDS WITH                     WXDCD   45
C              READ (INWTH,3005,END=3070)  (KARD(I),I=1,495)                    WXDCD   46
C              3005 FORMAT(5(99A1))                                             WXDCD   47
 3010 CONTINUE                                                                  WXDCD   48
      FIRST  = .FALSE.                                                          WXDCD   49
      IF (ITPTYP .EQ. 3)  GO TO 3020                                            WXDCD   50
      IHX    = IABS(IH-IBEGH)                                                   WXDCD   51
      IF (MOD(IHX,3) .EQ. 0)  GO TO 3020                                        WXDCD   52
      DO 1015 I=1,11                                                            WXDCD   53
 1015 IRAW(IH,I) = IUNDEF                                                       WXDCD   54
      GO TO 1040                                                                WXDCD   55
 3020 CONTINUE                                                                  WXDCD   56
      DO 1020 I = 1,8                                                           WXDCD   57
      ISR1   = ISTART + IRANGE(1,I)                                             WXDCD   58
      ISR2   = ISTART + IRANGE(2,I)                                             WXDCD   59
      IT     = 0                                                                WXDCD   60
      IF (ISR2 .NE. ISR1) IT = NUMB(KARD,ISTART+IRANGE(1,I),ISR2-1)             WXDCD   61
      CALL  EVALSN(IT,KARD(ISR2))                                               WXDCD   62
      ITEMP(I) = IT                                                             WXDCD   63
 1020 CONTINUE                                                                  WXDCD   64
C                                                                               WXDCD   65
C              TREAT SPECIAL FIELDS                                             WXDCD   66
C                                                                               WXDCD   67
C              CLOUD TYPE                                                       WXDCD   68
      ICLTY(IH) = ITEMP(8)                                                      WXDCD   69
      ICLTY1(IH) = ITEMP(8)                                                     WXDCD   70
      IF (ITEMP(8) .EQ. IUNDEF)  GO TO 3030                                     WXDCD   71
      ICT    = 0                                                                WXDCD   72
      IT     = IABS(IT)                                                         WXDCD   73
      IF (IT .LT. 8) ICT = 2                                                    WXDCD   74
      IF (IT .EQ. 2) ICT = 1                                                    WXDCD   75
      ICLTY(IH) = ICT                                                           WXDCD   76
 3030 CONTINUE                                                                  WXDCD   77
C                                                                               WXDCD   78
C              WIND DIRECTION                                                   WXDCD   79
      IF (ITEMP(5) .EQ. 0)  IWNDIR(IH) = 0                                      WXDCD   80
      IF (ITEMP(5) .EQ. 0)  GO TO 3050                                          WXDCD   81
      DO 1030 I = 1,16                                                          WXDCD   82
      IF (NWNDIR(I) .EQ. ITEMP(7)) GOTO 3040                                    WXDCD   83
 1030 CONTINUE                                                                  WXDCD   84
      IWNDIR(IH) = IUNDEF                                                       WXDCD   85
      GOTO 3050                                                                 WXDCD   86
 3040 CONTINUE                                                                  WXDCD   87
      IWNDIR(IH) = I - 1                                                        WXDCD   88
 3050 CONTINUE                                                                  WXDCD   89
C              CLOUD AMOUNT                                                     WXDCD   90
      IF (KARD(ISTART+IRANGE(2,6)) .EQ. IDASH) ITEMP(6) = 10                    WXDCD   91
      IF (ITEMP(6) .LT. 0)  ITEMP(6) = IUNDEF                                   WXDCD   92
C                                                                               WXDCD   93
C              SET RAIN AND SNOW FLAGS                                          WXDCD   94
      IT1    = 0                                                                WXDCD   95
      IT2    = 0                                                                WXDCD   96
      CALL  EVALSN(IT1,KARD(ISTART+68))                                         WXDCD   97
      CALL  EVALSN(IT2,KARD(ISTART+69))                                         WXDCD   98
      IF (IT1 .LT. 0 .AND. IT2 .LT. 0) IRN(IH) = IUNDEF                         WXDCD   99
      IF (IT1 .GT. 0 .OR. IT2 .GT. 0) IRN(IH) = 1                               WXDCD  100
      IT1    = 0                                                                WXDCD  101
      IT2    = 0                                                                WXDCD  102
      CALL  EVALSN(IT1,KARD(ISTART+70))                                         WXDCD  103
      CALL  EVALSN(IT2,KARD(ISTART+71))                                         WXDCD  104
      IF (IT1 .LT. 0 .AND. IT2 .LT. 0) ISN(IH) = IUNDEF                         WXDCD  105
      IF (IT1 .GT. 0 .OR. IT2 .GT. 0) ISN(IH) = 1                               WXDCD  106
C              TRANSFER DATA TO ACTUAL ARRAYS                                   WXDCD  107
      DO 1035 I = 1,6                                                           WXDCD  108
 1035 IRAW(IH,I) = ITEMP(I)                                                     WXDCD  109
      ISOL(IH) = IUNDEF                                                         WXDCD  110
      IDN(IH) = IUNDEF                                                          WXDCD  111
 1040 CONTINUE                                                                  WXDCD  112
      IDAYL  = NUMB(KARD,14,15)                                                 WXDCD  113
      RETURN                                                                    WXDCD  114
C                                                                               WXDCD  115
C              END-OF-FILE FOUND                                                WXDCD  116
 3070 CONTINUE                                                                  WXDCD  117
C              IBM********** INSERT THE FOLLOWING CARD                          WXDCD  118
C              IEOF = .TRUE.                                                    WXDCD  119
      RETURN                                                                    WXDCD  120
      END                                                                       WXDCD  121
      SUBROUTINE WXSTRT                                                         073b    86
C                                                                               WXSTRT   3
C              THIS ROUTINE FINDS THE START OF THE DESIRED YEAR ON THE          WXSTRT   4
C              CURRENT 1440 TAPE.  IT USES NON-STANDARD METHODS.  IN            WXSTRT   5
C              ORDER TO CONVERT TO STANDARD ONES, UN-COMMENT THE                WXSTRT   6
C              COMMENTED CODE.                                                  WXSTRT   7
C                                                                               WXSTRT   8
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /MONTHC/   BEFORE(12),MDAYS(12),MNAMES(12)                        /MONTHC/ 2
      INTEGER            BEFORE                                                 /MONTHC/ 3
C                                                                               /MONTHC/ 4
      COMMON  /PACKEC/   NTZ,ISTAT,XLAT,XLONG,                                  /PACKEC/ 2
     1                   IYR,INTINT,DDBT,IBEGH,NBS,IDOYSH(5),ISSHFT(5),         /PACKEC/ 3
     2                   ISHFT,ALT,SKYCOV(12),ILOC                              073a     1
C                                                                               /PACKEC/ 5
      COMMON  /PCKINT/   KYR,KSTAT,CN(12),TG(12),KTIM,KARD(500),KYRL            -089     1
C                                                                               /PCKINT/ 3
      COMMON  /REPORC/   IBEGM,IENDM                                            /REPORC/ 2
C                                                                               /REPORC/ 3
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
C                                                                               WXSTRT  17
      INTEGER RECSKP                                                            WXSTRT  20
C                                                                               WXSTRT  21
      IFAIL  = 0                                                                WXSTRT  22
 3000 CONTINUE                                                                  WXSTRT  23
      READ(INWTH,5001,END=3010) (KARD(I),I=1,495)                               WXSTRT  24
 5001 FORMAT(5(99A1))                                                           WXSTRT  25
      IF (IYR .EQ. IUNDEF)  GO TO 3005                                          WXSTRT  26
      KYR = NUMB ( KARD, 10, 11 ) + 1900                                        WXSTRT  27
      IF(KYR  .NE. IYR) GO TO 4000                                              WXSTRT  28
      GO TO 3009                                                                WXSTRT  29
3005  KSTAT = NUMB ( KARD, 5, 9 )                                               WXSTRT  30
      IF( ISTAT .NE. KSTAT )  GOTO 4000                                         WXSTRT  31
 3009 CONTINUE                                                                  WXSTRT  32
C              GET MONTH                                                        WXSTRT  33
      IM = NUMB ( KARD, 12, 13 )                                                WXSTRT  34
      IF (IM .GT. IBEGM)  GO TO 3010                                            WXSTRT  35
      IF (IM .NE. IBEGM) GO TO 4000                                             WXSTRT  36
C                                                                               WXSTRT  37
      RETURN                                                                    WXSTRT  38
 3010 CONTINUE                                                                  WXSTRT  39
      IEOF   = .TRUE.                                                           WXSTRT  40
      RETURN                                                                    WXSTRT  41
 4000 IF (IFAIL.EQ.1) GO TO 3000                                                WXSTRT  42
C   IF YOU ARE HERE, THEN YOU WISH TO SKIP UP TO START OF NEXT YEAR             WXSTRT  43
      IM = NUMB(KARD,12,13)                                                     WXSTRT  44
      IDY=NUMB(KARD,14,15)                                                      WXSTRT  45
      IHR=NUMB(KARD,16,17)                                                      WXSTRT  46
C   FIGURE HOW MANY RECORDS TO SKIP UP                                          WXSTRT  47
      RECSKP= 0                                                                 WXSTRT  48
      IMPRIM = 12-IM                                                            WXSTRT  49
      IF (IMPRIM.EQ.0) GO TO 4011                                               WXSTRT  50
C   WERE WE ALREADY IN DECEMBER<                                                WXSTRT  51
      DO 4010 I=1,IMPRIM                                                        WXSTRT  52
 4010 RECSKP=RECSKP+MDAYS(13-I)*4                                               WXSTRT  53
      IF (MOD(KYR,4).EQ.0.AND.IM.EQ.1) RECSKP=RECSKP+4                          WXSTRT  54
C   LEAP YEAR JANUARY --> MUST SKIP EXTRA DAY                                   WXSTRT  55
 4011 RECSKP=RECSKP+4*(MDAYS(IM)-IDY)                                           WXSTRT  56
      IF (MOD(KYR,4).EQ.0.AND.IM.EQ.2) RECSKP=RECSKP+4                          WXSTRT  57
C   LEAP YEAR FEBRUARY --> MUST SKIP EXTRA DAY                                  WXSTRT  58
      RECSKP=RECSKP+3-IHR/6                                                     WXSTRT  59
      IF (RECSKP.LE.0) GO TO 3000                                               WXSTRT  60
      IDYSKP=RECSKP/4                                                           WXSTRT  61
      RECSKP=RECSKP-IDYSKP*4                                                    WXSTRT  62
      DO 4020 I=1,RECSKP                                                        WXSTRT  63
 4020 READ (INWTH,5010,END=4030)                                                WXSTRT  64
 5010 FORMAT (5X)                                                               WXSTRT  65
      DO 4021 I=1,IDYSKP                                                        WXSTRT  66
 4021 READ (INWTH,5012,END=4030)                                                WXSTRT  67
 5012 FORMAT (///)                                                              WXSTRT  68
      READ (INWTH,5011,END=4030) IM,IDY,IHR                                     WXSTRT  69
 5011 FORMAT (9X,3I2)                                                           WXSTRT  70
      BACKSPACE INWTH                                                           WXSTRT  71
      IF (IM.EQ.1.AND.IDY.EQ.1.AND.IHR.EQ.0) GO TO 3000                         WXSTRT  72
C  THAT WAS A SUCCESSFUL SKIP.  HOWEVER, IF THINGS WENT WRONG....               WXSTRT  73
 4030 REWIND INWTH                                                              WXSTRT  74
      IFAIL = 1                                                                 WXSTRT  75
      GO TO 3000                                                                WXSTRT  76
      END                                                                       WXSTRT  77
      SUBROUTINE WY2DCD                                                         WY2DCD   2
C                                                                               WY2DCD   3
C Decode one day of weather data in WYEC2 format and                            WY2DCD   4
C store result in common block /RAWDAT/.                                        WY2DCD   5
C Derived from TMYDCD.                                                          WY2DCD   6
C                                                                               WY2DCD   7
      COMMON  /DAYPAR/   STSD,CTCD,SSS,CSC,HUNDOG,EQTIME,SOLCON,                /DAYPAR/ 2
     1                   TMSHFT,ISHFTH,WNXTHR,WPRVHR                            /DAYPAR/ 3
C                                                                               /DAYPAR/ 4
      COMMON  /FILES/    INFIL,OUTFIL,INWTH,OUTWTH,SOLWTH,STOUT                 /FILES/  2
      INTEGER            OUTFIL,OUTWTH,SOLWTH,STOUT                             /FILES/  3
C                                                                               /FILES/  4
      COMMON  /FILTYP/   IFTYP,IWSZ,ITPTYP                                      /FILTYP/ 2
C                                                                               /FILTYP/ 3
      COMMON  /GETCRC/   IEOF                                                   /GETCRC/ 2
      LOGICAL            IEOF                                                   /GETCRC/ 3
C                                                                               /GETCRC/ 4
      COMMON  /LOCALD/   STALAT,STALON,SSTALA,CSTALA,TSTALA,HRSLON              /LOCALD/ 2
C                                                                               /LOCALD/ 3
      COMMON  /PACKEC/   NTZ,ISTAT,XLAT,XLONG,                                  /PACKEC/ 2
     1                   IYR,INTINT,DDBT,IBEGH,NBS,IDOYSH(5),ISSHFT(5),         /PACKEC/ 3
     2                   ISHFT,ALT,SKYCOV(12),ILOC                              073a     1
C                                                                               /PACKEC/ 5
      COMMON  /PARAMS/   STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 2
      LOGICAL            STOPIT,VERS,FIRST,NEWPAK,CALCGT                        /PARAMS/ 3
C                                                                               /PARAMS/ 4
      COMMON  /PCKINT/   KYR,KSTAT,CN(12),TG(12),KTIM,KARD(500),KYRL            -089     1
C                                                                               /PCKINT/ 3
      COMMON  /RAWDAT/   IDRY(24),IWET(24),IDEW(24),IPRESS(24),                 /RAWDAT/ 2
     1                   IWNDSP(24),ICLAMT(24),ISOL(24),IDN(24),                /RAWDAT/ 3
     2                   IWNDIR(24),ICLTY(24),IRN(24),ISN(24),                  CD144S   1
     3                   IOPQCA(24),IRELH(24),ICLTY1(24)                        CD144S   2
      DIMENSION IRAW(24,14)                                                     CD144S   3
      EQUIVALENCE        (IDRY(1),IRAW(1,1))                                    /RAWDAT/ 6
      COMMON /LSTIME/   LSTHRS(24)                                              /RAWDAT/ 7
C                                                                               /RAWDAT/ 8
      COMMON  /REPORC/   IBEGM,IENDM                                            /REPORC/ 2
C                                                                               /REPORC/ 3
      COMMON  /TIMES/    IMNTH,IDAY,IHOUR,IRECXO,IDXO,IDAYL,ITIM                073b     2
C                                                                               /TIMES/  3
      COMMON  /TMYME/    IM,KDOM,IH                                             /TMYME/  2
C                                                                               /TMYME/  3
      COMMON  /UNDEF/    IUNDEF,UNDEF                                           /UNDEF/  2
C                                                                               /UNDEF/  3
C                                                                               WY2DCD  21
      REAL WBD                                                                  -089   116
      SAVE WBD                                                                  -089   117
      DIMENSION KW(8)                                                           WY2DCD  22
      DATA NOT2,NOT3,NOT4,NOT5/99,999,9999,99999/                               WY2DCD  23
      DATA  WBD      /0.0/                                                      WY2DCD  24
C                                                                               WY2DCD  25
C                                                                               WY2DCD  26
 3000 CONTINUE                                                                  WY2DCD  27
      IF (IDAY .EQ. IDAYL)  RETURN                                              WY2DCD  28
      DO 1040 IH=1,24                                                           WY2DCD  29
C              READ NEXT HOUR AND CONVERT WYEC2 DATA                            WY2DCD  30
      READ(INWTH,9010,END=6000) KSTAT,KYR,IM,KDOM,KHR,KSOL,KDN,                 WY2DCD  31
     .  (KW(I),I=1,8),KPRESS,KDB,KDP,KWNDIR,KWNDSP,KCOV,KOPQ                    WY2DCD  32
 9010 FORMAT (I5,1X,4I2,4X,I4,2X,I4,46X,                                        WY2DCD  33
     .  8I1,1X,I5,1X,I4,1X,I4,1X,I3,1X,I4,1X,I2,1X,I2)                          WY2DCD  34
                                                                                WY2DCD  35
      IF (KSTAT .NE. ISTAT) GO TO 3000                                          WY2DCD  36
      IF (IM    .LT. IBEGM) GO TO 3000                                          WY2DCD  37
      FIRST  = .FALSE.                                                          WY2DCD  38
C              INITIALIZE SNOW AND RAIN FLAGS                                   WY2DCD  39
      IRN(IH) = 0                                                               WY2DCD  40
      ISN(IH) = 0                                                               WY2DCD  41
                                                                                WY2DCD  42
      ISOL(IH) = IFIX(.08807 * FLOAT(KSOL) + 0.5)                               WY2DCD  43
      IF (KSOL .EQ. NOT4)  ISOL(IH) = 0.                                        WY2DCD  44
                                                                                WY2DCD  45
      IDN(IH) = IFIX(.08807 * FLOAT(KDN) + 0.5)                                 WY2DCD  46
      IF (KDN .EQ. NOT4)  IDN(IH) = 0.                                          WY2DCD  47
                                                                                WY2DCD  48
      IF (KDB .EQ. NOT4) GO TO 1010                                             WY2DCD  49
      TDB    = .18 * FLOAT(KDB) + 32.                                           WY2DCD  50
      IDRY(IH) = IROUND(TDB)                                                    -089   118
      GO TO 1011                                                                WY2DCD  52
 1010 IDRY(IH) = IUNDEF                                                         WY2DCD  53
                                                                                WY2DCD  54
 1011 IF (KDP .EQ. NOT4)  GO TO 1012                                            WY2DCD  55
      IF (KDP .GT. KDB)   KDP = KDB                                             WY2DCD  56
      TDP    = .18 * FLOAT(KDP) + 32.                                           WY2DCD  57
      IDEW(IH) = IROUND(TDP)                                                    -089   119
      GO TO 1013                                                                WY2DCD  59
 1012 IDEW(IH) = IUNDEF                                                         WY2DCD  60
                                                                                WY2DCD  61
 1013 IF (KPRESS .EQ. NOT5)  GO TO 1014                                         WY2DCD  62
      BP     = .002953 * FLOAT(KPRESS)                                          WY2DCD  63
      IPRESS(IH) = IFIX(100.*BP + .5)                                           WY2DCD  64
      GO TO 1015                                                                WY2DCD  65
 1014 IPRESS(IH) = IUNDEF                                                       WY2DCD  66
 1015 IF (KDB .EQ. NOT4 .OR. KDP .EQ. NOT4 .OR. KPRESS .EQ. NOT5)               WY2DCD  67
     1  GO TO 1016                                                              WY2DCD  68
      TWB    = SLVWBT(BP,TDB,TDP,TDB-WBD)                                       -089   120
      WBD    = TDB - TWB                                                        WY2DCD  70
      IWET(IH) = IROUND(TWB)                                                    -089   121
      GO TO 1017                                                                WY2DCD  72
 1016 IWET(IH) = IUNDEF                                                         WY2DCD  73
 1017 CONTINUE                                                                  WY2DCD  74
      IF ((KW(7).GT.0 .AND. KW(7).LT.9) .OR. (KW(6).GT.0 .AND. KW(6).LT.        WY2DCD  75
     1 7) .OR. (KW(3).GT.0 .AND. KW(3).LT.9))  IRN(IH) = 1                      WY2DCD  76
      IF ((KW(5).GT.0 .AND. KW(5).LT.9)                                         WY2DCD  77
     1 .OR. (KW(4).GT.0 .AND. KW(4).LT.7))  ISN(IH) = 1                         WY2DCD  78
      IWNDSP(IH) = IFIX(FLOAT(KWNDSP) * .1944 + .5)                             WY2DCD  79
      IF (KWNDSP .EQ. NOT4)  IWNDSP(IH) = IUNDEF                                WY2DCD  80
      IF (KWNDIR .EQ. NOT3)  IWNDIR(IH) = IUNDEF                                WY2DCD  81
C              SET WIND DIRECTION TO 0-15                                       WY2DCD  82
      IF (IWNDSP(IH) .EQ. 0)  IWNDIR(IH) = 0                                    WY2DCD  83
      IF (IWNDSP(IH) .EQ. 0)  GO TO 1020                                        WY2DCD  84
      IF (IWNDIR(IH) .EQ. IUNDEF)  GO TO 1020                                   WY2DCD  85
      IWNDIR(IH) = INT(FLOAT(KWNDIR)*4.444444E-2 + 0.5)                         WY2DCD  86
      IF (IWNDIR(IH) .EQ. 16)  IWNDIR(IH) = 0                                   WY2DCD  87
 1020 CONTINUE                                                                  WY2DCD  88
      IF (KOPQ .EQ. 0)  GO TO 1030                                              WY2DCD  89
      IF (KOPQ .EQ. NOT2)  GO TO 1030                                           WY2DCD  90
C              KOPQ .GT. 0                                                      WY2DCD  91
      ICLTY(IH)  = 2                                                            WY2DCD  92
      ICLAMT(IH) = KOPQ                                                         WY2DCD  93
      GO TO 1040                                                                WY2DCD  94
C              KOPQ ZERO OR NOT THERE                                           WY2DCD  95
 1030 ICLTY(IH) = 2                                                             WY2DCD  96
      IF (KOPQ .EQ. 0)  ICLTY(IH) = 1                                           WY2DCD  97
      IF (KCOV .EQ. NOT2)  ICLTY(IH) = IUNDEF                                   WY2DCD  98
      ICLAMT(IH) = KCOV                                                         WY2DCD  99
      IF (KCOV .EQ. NOT2)  ICLAMT(IH) = IUNDEF                                  WY2DCD 100
      ICLTY1(IH) = IUNDEF                                                       WY2DCD 101
 1040 CONTINUE                                                                  WY2DCD 102
      IDAYL  = KDOM                                                             WY2DCD 104
      RETURN                                                                    WY2DCD 105
 6000 IEOF   = .TRUE.                                                           WY2DCD 106
      RETURN                                                                    WY2DCD 107
      END                                                                       WY2DCD 108
