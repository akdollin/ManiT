; ModuleID = 'ManiT'

%pictures = type { i8* }

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@pic1 = global i8* null
@0 = private unnamed_addr constant [239 x i8] c"__    __   ___________    ____  \0A|  |  |  | |   ____\5C   \5C  /   /  \0A|  |__|  | |  |__   \5C   \5C/   /   \0A|   __   | |   __|   \5C_    _/    \0A|  |  |  | |  |____    |  |   __ \0A|__|  |__| |_______|   |__|  (_ )\0A                              |/ \0A\0A\00"
@pic2 = global i8* null
@1 = private unnamed_addr constant [1024 x i8] c"     _______.___________. _______ .______    __    __   _______ .__   __.           \0A    /       |           ||   ____||   _  \5C  |  |  |  | |   ____||  \5C |  |           \0A   |   (----`---|  |----`|  |__   |  |_)  | |  |__|  | |  |__   |   \5C|  |           \0A    \5C   \5C       |  |     |   __|  |   ___/  |   __   | |   __|  |  . `  |           \0A.----)   |      |  |     |  |____ |  |      |  |  |  | |  |____ |  |\5C   |           \0A|_______/       |__|     |_______|| _|      |__|  |__| |_______||__| \5C__|           \0A\0A\0A _______  _______  ____    __    ____  ___      .______       _______       _______.\0A|   ____||       \5C \5C   \5C  /  \5C  /   / /   \5C     |   _  \5C     |       \5C     /       |\0A|  |__   |  .--.  | \5C   \5C/    \5C/   / /  ^  \5C    |  |_)  |    |  .--.  |   |   (----`\0A|   __|  |  |  |  |  \5C            / /  /_\5C  \5C   |      /     |  |  |  |    \5C   \5C    \0A|  |____ |  '--'  |   \5C    /\5C    / /  _____  \5C  |  |\5C  \5C----.|  '--'  |.----)   |   \0A|_______||_______/     \5C__/  \5C__/ /__/     \5C__\5C | _| `._____||_______/ |_______/    \0A\0A\00"
@pic3 = global i8* null
@2 = private unnamed_addr constant [73 x i8] c"\0A\0A .-.     .-.     .-.  \0A(   )   (   )   (   ) \0A `-'     `-'     `-'  \0A\0A\00"
@pic4 = global i8* null
@3 = private unnamed_addr constant [372 x i8] c"88888b   d888b  88b  88 8 888888    88888b   888    88b  88 88  d888b  88\0A88   88 88   88 888b 88 P   88      88   88 88 88   888b 88 88 88   '  88\0A88   88 88   88 88'8b88     88      88888P 88   88  88'8b88 88 88      88\0A88   88 88   88 88 '888     88      88    d8888888b 88 '888 88 88   ,  ''\0A88888P   T888P  88  '88     88      88    88     8b 88  '88 88  T888P  88\0A\0A\00"
@pic5 = global i8* null
@4 = private unnamed_addr constant [2517 x i8] c"MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM\0AMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM 'MM' VMMMMM\0AMMMMMV  MV  MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM  VM  MMMMMM\0AMMMMMM  M  mMMMMMMMMMMMMMMMMMMMV''     ''VMMMMMMMMMMMMMM  MMMA 'M  MM  MM\0AMM  VM  M  MMMMMMMMMMMMMMMV'                 'VMMMMMMMMM.  'MM  M  M' .MM\0AMM.  M  M  MV  VMMMMMMMV'                      'VMMMMMMMMM.  '  V  V .MMM\0AMMA  V  M  M' ,MMMMMMV'                          'VMMMMMMMM.  ..     mMMM\0AMMMA '     V  MMMMMM'                              'VMMMMMMm  'S'   mMMMM\0AMMMM  .,.,   AMMMMV                                 'VMMM'''   :   .MMMMM\0AMMMM  'B'   MMMMMV                                    M'      .'  .MMMMMM\0AMMMM   :   AV'  V                                     '   .mm.    MMMMMMM\0AMMMM.  '.                                              ..MMMMMm   MMMMMMM\0AMMMMM.. .  .mMMV  .                                   . VMMMMMMA   VMMMMM\0AMMMMMM  AMMMMMM'  *         <^@^>        <==>        .* 'MMMMMMm    MMMMM\0AMMMMM'  MMMMMMV  .I                                 .a@. V''MMMMA    MMMM\0AMMMMM   MMMMMM(  a@:.                             .' @@! .   'MMMm    MMM\0AMMMM'   MMMV'''  !@a :.                         .';.a@@R ,             MM\0AMMMV    MV'   :  :@@@: :.                     .:  a@@@@! ..............mM\0AMMM'          .  '@@@@ : '...             ..:' : a@@@@@' MMMMMMMMMMMMMMMM\0AMMM  ..........   @@@@@a  :  :'':'------':  :  a@@@@@@@  MMMMMMMMMMMMMMMM\0AMMMMMMMMMMMMMMA   '@@@@@@@a  :  :   ::   :  a@@@@@@@@@' :MMMMMMMMMMMMMMMM\0AMMMMMMMMMMMMMMM.   '@@@@@@@@@@aaA. .;|. .Aaa@@@@@@@@@' .AMMMMMMMMMMMMMMMM\0AMMMMMMMMMMMMMMMM.   '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   mMMMMMMMMMMMMMMMMM\0AMMMMMMMMMMMMMMMMM.    @@@@@@@@@@@'oOo.oOOo'@@@@@@@'   mMMMMMMMMMMMMMMMMMM\0AMMMMMMMMMMMMMMMMMMm    '@@@@@@@'OOOOOOxOOOOO'@@@V    mMMMMMMMMMMMMMMMMMMM\0AMMMMMMMMMMMMMMMMMMMA.     '@@@'OOOOOOOOxOOOOO'@'   .AMMMMMMMMMMMMMMMMMMMM\0AMMMMMMMMMMMMMMMMMMMMMA.     ''V@@AOOOOOOxOOOOO.  .AMMMMMMMMMMMMMMMMMMMMMM\0AMMMMMMMMMMMMMMMMMMMMMMMMm.       'OOOOOOOxXOOOo.mMMMMMMMMMMMMMMMMMMMMMMMM\0AMMMMMMMMMMMMMMMMMMMMMMMMMMMAm..   'OOOOOOoxOOOO:MMMMMMMMMMMMMMMMMMMMMMMMM\0AMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMA'OOOOOOOxOOOO:MMMMMMMMMMMMMMMMMMMMMMMMM\0AMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMA'OOOOOOxOOOO;MMMMMMMMMMMMMMMMMMMMMMMMM\0AMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMA'OOOOOOOOO;AMMMMMMMMMMMMMMMMMMMMMMMMM\0AMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMA'OOOOOO;AMMMMMMMMMMMMMMMMMMMMMMMMMMM\0AWIZMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMmmmmmmMMMMMMMMMMMMMMMMMMMMMMMMM*MJJ\0A\00"
@p = global %pictures zeroinitializer
@5 = private unnamed_addr constant [347 x i8] c"\0A\0A\0A\0A  _                     \0A | |    _____   _____   \0A | |   / _ \5C \5C / / _ \5C  \0A | |__| (_) \5C V /  __/_ \0A |_____\5C___/ \5C_/ \5C___( )\0A                     |/ \0A\0A          __  __             _ _____ \0A         |  \5C/  | __ _ _ __ (_)_   _|\0A         | |\5C/| |/ _` | '_ \5C| | | |  \0A         | |  | | (_| | | | | | | |  \0A         |_|  |_|\5C__,_|_| |_|_| |_|  \0A\0A\00"
@6 = private unnamed_addr constant [9 x i8] c"pic1.txt\00"
@7 = private unnamed_addr constant [4 x i8] c"cat\00"
@8 = private unnamed_addr constant [4 x i8] c"cat\00"
@9 = private unnamed_addr constant [9 x i8] c"pic2.txt\00"
@10 = private unnamed_addr constant [4 x i8] c"cat\00"
@11 = private unnamed_addr constant [4 x i8] c"cat\00"
@12 = private unnamed_addr constant [9 x i8] c"pic3.txt\00"
@13 = private unnamed_addr constant [4 x i8] c"cat\00"
@14 = private unnamed_addr constant [4 x i8] c"cat\00"
@15 = private unnamed_addr constant [9 x i8] c"pic4.txt\00"
@16 = private unnamed_addr constant [4 x i8] c"cat\00"
@17 = private unnamed_addr constant [4 x i8] c"cat\00"
@18 = private unnamed_addr constant [9 x i8] c"pic5.txt\00"
@19 = private unnamed_addr constant [4 x i8] c"cat\00"
@20 = private unnamed_addr constant [4 x i8] c"cat\00"
@21 = private unnamed_addr constant [9 x i8] c"pic6.txt\00"
@22 = private unnamed_addr constant [4 x i8] c"cat\00"
@23 = private unnamed_addr constant [4 x i8] c"cat\00"
@24 = private unnamed_addr constant [2 x i8] c"w\00"
@25 = private unnamed_addr constant [9 x i8] c"pic1.txt\00"
@26 = private unnamed_addr constant [2 x i8] c"w\00"
@27 = private unnamed_addr constant [9 x i8] c"pic2.txt\00"
@28 = private unnamed_addr constant [2 x i8] c"w\00"
@29 = private unnamed_addr constant [9 x i8] c"pic3.txt\00"
@30 = private unnamed_addr constant [2 x i8] c"w\00"
@31 = private unnamed_addr constant [9 x i8] c"pic4.txt\00"
@32 = private unnamed_addr constant [2 x i8] c"w\00"
@33 = private unnamed_addr constant [9 x i8] c"pic5.txt\00"
@34 = private unnamed_addr constant [2 x i8] c"w\00"
@35 = private unnamed_addr constant [9 x i8] c"pic6.txt\00"

declare i32 @printf(i8*, ...)

declare i8* @fopen(i8*, i8*)

declare i32 @fclose(i8*)

declare i32 @fputs(i32, i8*)

declare i8* @fgets(i8*, i32, i8*)

declare i32 @fwrite(i8*, i32, i32, i8*)

declare i32 @strlen(i8*)

declare i32 @fork()

declare i32 @sleep(i32)

declare i32 @execlp(i8*, i8*, i8*, i32)

define i32 @main() {
entry:
  store i8* getelementptr inbounds ([239 x i8], [239 x i8]* @0, i32 0, i32 0), i8** @pic1
  store i8* getelementptr inbounds ([1024 x i8], [1024 x i8]* @1, i32 0, i32 0), i8** @pic2
  store i8* getelementptr inbounds ([73 x i8], [73 x i8]* @2, i32 0, i32 0), i8** @pic3
  store i8* getelementptr inbounds ([372 x i8], [372 x i8]* @3, i32 0, i32 0), i8** @pic4
  store i8* getelementptr inbounds ([2517 x i8], [2517 x i8]* @4, i32 0, i32 0), i8** @pic5
  store i8* getelementptr inbounds ([347 x i8], [347 x i8]* @5, i32 0, i32 0), i8** getelementptr inbounds (%pictures, %pictures* @p, i32 0, i32 0)
  %demo_result = call i32 @demo()
  %tmp = icmp eq i32 %demo_result, 1
  br i1 %tmp, label %then, label %else39

merge:                                            ; preds = %else39, %merge33
  ret i32 0

then:                                             ; preds = %entry
  %sleep = call i32 @sleep(i32 1)
  %pid = alloca i32
  %fork = call i32 @fork()
  store i32 %fork, i32* %pid
  %pid1 = load i32, i32* %pid
  %tmp2 = icmp eq i32 %pid1, 0
  br i1 %tmp2, label %then4, label %else

merge3:                                           ; preds = %else, %then4
  %sleep5 = call i32 @sleep(i32 1)
  %fork6 = call i32 @fork()
  store i32 %fork6, i32* %pid
  %pid7 = load i32, i32* %pid
  %tmp8 = icmp eq i32 %pid7, 0
  br i1 %tmp8, label %then10, label %else12

then4:                                            ; preds = %then
  %execlp = call i32 @execlp(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @8, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @7, i32 0, i32 0), i8* getelementptr inbounds ([9 x i8], [9 x i8]* @6, i32 0, i32 0), i32 0)
  br label %merge3

else:                                             ; preds = %then
  br label %merge3

merge9:                                           ; preds = %else12, %then10
  %sleep13 = call i32 @sleep(i32 1)
  %fork14 = call i32 @fork()
  store i32 %fork14, i32* %pid
  %pid15 = load i32, i32* %pid
  %tmp16 = icmp eq i32 %pid15, 0
  br i1 %tmp16, label %then18, label %else20

then10:                                           ; preds = %merge3
  %execlp11 = call i32 @execlp(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @11, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @10, i32 0, i32 0), i8* getelementptr inbounds ([9 x i8], [9 x i8]* @9, i32 0, i32 0), i32 0)
  br label %merge9

else12:                                           ; preds = %merge3
  br label %merge9

merge17:                                          ; preds = %else20, %then18
  %sleep21 = call i32 @sleep(i32 2)
  %fork22 = call i32 @fork()
  store i32 %fork22, i32* %pid
  %pid23 = load i32, i32* %pid
  %tmp24 = icmp eq i32 %pid23, 0
  br i1 %tmp24, label %then26, label %else28

then18:                                           ; preds = %merge9
  %execlp19 = call i32 @execlp(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @14, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @13, i32 0, i32 0), i8* getelementptr inbounds ([9 x i8], [9 x i8]* @12, i32 0, i32 0), i32 0)
  br label %merge17

else20:                                           ; preds = %merge9
  br label %merge17

merge25:                                          ; preds = %else28, %then26
  %sleep29 = call i32 @sleep(i32 1)
  %fork30 = call i32 @fork()
  store i32 %fork30, i32* %pid
  %pid31 = load i32, i32* %pid
  %tmp32 = icmp eq i32 %pid31, 0
  br i1 %tmp32, label %then34, label %else36

then26:                                           ; preds = %merge17
  %execlp27 = call i32 @execlp(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @17, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @16, i32 0, i32 0), i8* getelementptr inbounds ([9 x i8], [9 x i8]* @15, i32 0, i32 0), i32 0)
  br label %merge25

else28:                                           ; preds = %merge17
  br label %merge25

merge33:                                          ; preds = %else36, %then34
  %sleep37 = call i32 @sleep(i32 1)
  %execlp38 = call i32 @execlp(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @23, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @22, i32 0, i32 0), i8* getelementptr inbounds ([9 x i8], [9 x i8]* @21, i32 0, i32 0), i32 0)
  br label %merge

then34:                                           ; preds = %merge25
  %execlp35 = call i32 @execlp(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @20, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @19, i32 0, i32 0), i8* getelementptr inbounds ([9 x i8], [9 x i8]* @18, i32 0, i32 0), i32 0)
  br label %merge33

else36:                                           ; preds = %merge25
  br label %merge33

else39:                                           ; preds = %entry
  br label %merge
}

define i32 @demo() {
entry:
  %i = alloca i32
  store i32 0, i32* %i
  br label %while

while:                                            ; preds = %merge, %entry
  %i67 = load i32, i32* %i
  %tmp68 = icmp slt i32 %i67, 6
  br i1 %tmp68, label %while_body, label %merge69

while_body:                                       ; preds = %while
  %pid = alloca i32
  %fork = call i32 @fork()
  store i32 %fork, i32* %pid
  %pid1 = load i32, i32* %pid
  %tmp = icmp eq i32 %pid1, 0
  br i1 %tmp, label %then, label %else64

merge:                                            ; preds = %else64, %merge4
  %i65 = load i32, i32* %i
  %tmp66 = add i32 %i65, 1
  store i32 %tmp66, i32* %i
  br label %while

then:                                             ; preds = %while_body
  %i2 = load i32, i32* %i
  %tmp3 = icmp eq i32 %i2, 0
  br i1 %tmp3, label %then5, label %else

merge4:                                           ; preds = %merge11
  br label %merge

then5:                                            ; preds = %then
  %f1 = alloca i8*
  %fopen = call i8* @fopen(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @25, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @24, i32 0, i32 0))
  store i8* %fopen, i8** %f1
  %f16 = load i8*, i8** %f1
  %pic1 = load i8*, i8** @pic1
  %strlen = call i32 @strlen(i8* %pic1)
  %pic17 = load i8*, i8** @pic1
  %fwrite = call i32 @fwrite(i8* %pic17, i32 %strlen, i32 1, i8* %f16)
  %f18 = load i8*, i8** %f1
  %fclose = call i32 @fclose(i8* %f18)
  ret i32 0

else:                                             ; preds = %then
  %i9 = load i32, i32* %i
  %tmp10 = icmp eq i32 %i9, 1
  br i1 %tmp10, label %then12, label %else20

merge11:                                          ; preds = %merge23
  br label %merge4

then12:                                           ; preds = %else
  %f2 = alloca i8*
  %fopen13 = call i8* @fopen(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @27, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @26, i32 0, i32 0))
  store i8* %fopen13, i8** %f2
  %f214 = load i8*, i8** %f2
  %pic2 = load i8*, i8** @pic2
  %strlen15 = call i32 @strlen(i8* %pic2)
  %pic216 = load i8*, i8** @pic2
  %fwrite17 = call i32 @fwrite(i8* %pic216, i32 %strlen15, i32 1, i8* %f214)
  %f218 = load i8*, i8** %f2
  %fclose19 = call i32 @fclose(i8* %f218)
  ret i32 0

else20:                                           ; preds = %else
  %i21 = load i32, i32* %i
  %tmp22 = icmp eq i32 %i21, 2
  br i1 %tmp22, label %then24, label %else32

merge23:                                          ; preds = %merge35
  br label %merge11

then24:                                           ; preds = %else20
  %f3 = alloca i8*
  %fopen25 = call i8* @fopen(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @29, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @28, i32 0, i32 0))
  store i8* %fopen25, i8** %f3
  %f326 = load i8*, i8** %f3
  %pic3 = load i8*, i8** @pic3
  %strlen27 = call i32 @strlen(i8* %pic3)
  %pic328 = load i8*, i8** @pic3
  %fwrite29 = call i32 @fwrite(i8* %pic328, i32 %strlen27, i32 1, i8* %f326)
  %f330 = load i8*, i8** %f3
  %fclose31 = call i32 @fclose(i8* %f330)
  ret i32 0

else32:                                           ; preds = %else20
  %i33 = load i32, i32* %i
  %tmp34 = icmp eq i32 %i33, 3
  br i1 %tmp34, label %then36, label %else44

merge35:                                          ; preds = %merge47
  br label %merge23

then36:                                           ; preds = %else32
  %f4 = alloca i8*
  %fopen37 = call i8* @fopen(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @31, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @30, i32 0, i32 0))
  store i8* %fopen37, i8** %f4
  %f438 = load i8*, i8** %f4
  %pic4 = load i8*, i8** @pic4
  %strlen39 = call i32 @strlen(i8* %pic4)
  %pic440 = load i8*, i8** @pic4
  %fwrite41 = call i32 @fwrite(i8* %pic440, i32 %strlen39, i32 1, i8* %f438)
  %f442 = load i8*, i8** %f4
  %fclose43 = call i32 @fclose(i8* %f442)
  ret i32 0

else44:                                           ; preds = %else32
  %i45 = load i32, i32* %i
  %tmp46 = icmp eq i32 %i45, 4
  br i1 %tmp46, label %then48, label %else56

merge47:                                          ; No predecessors!
  br label %merge35

then48:                                           ; preds = %else44
  %f5 = alloca i8*
  %fopen49 = call i8* @fopen(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @33, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @32, i32 0, i32 0))
  store i8* %fopen49, i8** %f5
  %f550 = load i8*, i8** %f5
  %pic5 = load i8*, i8** @pic5
  %strlen51 = call i32 @strlen(i8* %pic5)
  %pic552 = load i8*, i8** @pic5
  %fwrite53 = call i32 @fwrite(i8* %pic552, i32 %strlen51, i32 1, i8* %f550)
  %f554 = load i8*, i8** %f5
  %fclose55 = call i32 @fclose(i8* %f554)
  ret i32 0

else56:                                           ; preds = %else44
  %f6 = alloca i8*
  %fopen57 = call i8* @fopen(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @35, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @34, i32 0, i32 0))
  store i8* %fopen57, i8** %f6
  %f658 = load i8*, i8** %f6
  %struct_access = load i8*, i8** getelementptr inbounds (%pictures, %pictures* @p, i32 0, i32 0)
  %strlen59 = call i32 @strlen(i8* %struct_access)
  %struct_access60 = load i8*, i8** getelementptr inbounds (%pictures, %pictures* @p, i32 0, i32 0)
  %fwrite61 = call i32 @fwrite(i8* %struct_access60, i32 %strlen59, i32 1, i8* %f658)
  %f662 = load i8*, i8** %f6
  %fclose63 = call i32 @fclose(i8* %f662)
  ret i32 0

else64:                                           ; preds = %while_body
  br label %merge

merge69:                                          ; preds = %while
  ret i32 1
}
