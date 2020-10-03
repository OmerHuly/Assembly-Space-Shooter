.MODEL SMALL        ;model type (small means Code & Data have seperate segment, but must be each less than 64k Both Code and Data are NEAR.

.STACK 200h         ;200h in stack

.DATA               ;some data... (DS)

EnemiesInGame equ 0FFh  ;change here how much max enemies will be (in 20x)
rowConst equ 1          ;how many rows of the same enemy
starSpeedConst equ 1    ;how much runs for stars to fall 1 pixel down
enemyMoveConst equ 10   ;how many runs for the enemies to move 1 pixel
BoomTimeConst equ 10    ;how many runs the boom will stay on screen
LaserConst equ 2    ;how many enemies will the laser (red gun) kill
StartLives equ 3    ;default lives
maxNav equ 2        ;how much options in the main menu (maxNav+1 options)
Names db 500 dup (?)    ;all the names in the scoreboard (name, 0D, name, 0D...)
ScoresInBoard db 100 dup (?);all the scores in the scoreboard
BoardPlace dw 1     ;place in the scoreboard (1-20)
PowerupTimeConst equ 0FFFFh ;how much time will a powerup stay on constant
Scores dw 0         ;scores
ScoresString db '00000' ;scores as a string of five characters
LivesString db '000'    ;lives as a string of three characters
BigDelayTime db 100 ;how long big delay
cursorPage db 0     ;cursor page
starLoc dw 201 dup (0FFFFh) ;every star on screen location
starCount db 0      ;how many stars on screen
starSpeed db starSpeedConst
;StarStart dw 9600   ;stars start row (30)
;StarEnd dw 60800    ;stars last row (190)
playerLoc dw 60640  ;player's location
playerSpeed dw 1    ;player's speed in pixels per run
playerBulletLoc dw 0;player's bullet location
playerShot db 0     ;0 - has not shot, 1 - has shot
Lives db 3          ;how many lives does the player have
InGameLives db 3    ;based on lives and keep it as you progress in levels
bulletSpeed dw 960  ;speed of player and enemies' bullets (in 320x)
gun db Gray         ;gun powerup
PowerupTime dw 0FFFFh   ;how much time will a powerup stay on
Laser db Red
enemyCount db EnemiesInGame         ;how much enemies?
enemiesInLevel db ?                 ;how much enemies in the start of level
enemyLoc dw EnemiesInGame dup (0)   ;every enemy on screen location
enemyLocEnd dw ?    ;enemyLoc end
enemyHorizontalSpeed dw 1   ;enemy's horizontal speed in pixels per run
enemyVerticalSpeed dw 320   ;enemy's vertical speed in rows per some runs (in 320x)
enemyType dw EnemiesInGame dup (0)  ;enemy type
enemyBulletLoc dw 3 dup (0)         ;enemies' bullet location (up to 3 bullets)
enemyShootLimit dw 19000    ;100 - 100*enemyShootLimit/19682 = chance to shoot
enemyDead db 0      ;0 - enemy not dead, 1 - enemy dead
enemyAnimation db 0 ;0 - position 0, 1 - position 1
BoomLoc dw 0        ;Boom location
BoomTime db 10      ;time for the Boom to stay on screen
BoomSound dw 17218  ;Boom sound
row db rowConst     ;how many rows of the same enemy
enemyDir db 1       ;enemies diraction: 0 = left, 1 = right
enemyMove db 1      ;how many runs for the enemies to move 1
BonusLimit dw 19660 ;100 - 100*BonusLimit/19682 = chance for bonus to spawn
BonusExist db 0     ;0 - not exist, 1 - exist
BonusLoc dw ?       ;where is the bonus on the screen
BonusLocConstant equ 10877  ;row 33 (33*320) right side (+320) -3 pixels
BonusHorizontalSpeed dw 1   ;Bonus' horizontal speed in pixels per run
BonusSound equ 3620     ;the sound of the enemys move
BonusSound2 equ 4561    ;the sound of the enemys second move
White equ 15        ;white color
Black equ 0         ;Black color
Green equ 47        ;Green color
Gray equ 8          ;Gray color
Red equ 40          ;Red color
Orange equ 42       ;Orange color
Blue equ 55         ;Blue color
Yellow equ 44       ;Yellow color
rand dw ?           ;random number
navigation db 0     ;0 - start game, 1 - quit game
Level db 1          ;current level
FirstRun db 0       ;first run of the main loop, 0 - not the first run
Win db 1            ;when the game end if 1 - you won! else - you lost...
note dw 0           ;1193180 / frequency (for playing sounds)
clock equ es:6Ch    ;clock
SpeakerON db 0      ;0 - off, 1 - on
ShotSound dw 0      ;the sound of the player's shot when fired
ShotSoundConstant equ 2048  ;the sound of the player's shot when reset
ShotSoundCounter db 0   ;for how many runs will the player's shot make a sound
ShotSoundCounterConstant equ 2      ;value when resetting the shot sound
;ShotSoundInc equ 1  ;by how much increase the shot's sound note
EnemySound equ 7240     ;the sound of the enemys move
EnemySound2 equ 7670    ;the sound of the enemys second move
Seconds db '00'     ;play time Seconds
Minutes db '00'     ;play time Minutes
Hours db '00'       ;play time Hours
MinutesHEX db 0     ;Minutes variable
HoursHEX db 0       ;Hours variable
StartSeconds db 0   ;play time Starting Seconds
;StartMinutes db 0   ;play time Starting Minutes
;StartHour db 0      ;play time Starting Hour
FirstTime db 1      ;1 - first time, 0 - not the first time (for time displaying)
MinutesCanPass db 0 ;0 - can not pass, 1 - can pass
HoursCanPass db 0   ;0 - can not pass, 1 - can pass
FileName db 'Data.txt', 0
Handle dw ?
offsetLOW dw ?      ;offset from file start
offsetHIGH dw ?     ;offset from file start
ScoreData db 736 dup (0)
;-----------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:-------------------:--:--
GameData db 23 dup (0)

.CODE               ;CS starts here

;macro area:

;********************************************************************

;this macro prints a string that is given as a parameter, example:
;PRINT 'hello world!'
PRINT macro string
local next_char, msg, printed, skip, msgEnd

    push ax         ;store registers...
    push bx
    push cx
    push dx
    push si
    push es

    jmp skip        ;skip declaration.
    msg db string;, 0
    msgEnd db ?

;INT 10h / AH = 13h - write string
;AL = write mode:
; bit 0: update cursor after writing;
; bit 1: string contains attributes.
;BH = page number.
;BL = attribute if string contains only characters (bit 1 of AL is zero).
;CX = number of characters in string (attributes are not counted).
;DL,DH = column, row at which to start writing.
;ES:BP points to string to be printed.

skip:
    ;lea si, msg

next_char:
    ;mov al, cs:[si]
    ;cmp al, 0
    ;jz  printed
    ;inc si
    ;mov ah, 0Eh     ;teletype function.
    ;int 10h
    ;jmp next_char
    mov bh, cursorPage
    mov ah, 3
    int 10h         ;get cursor position
    lea cx, msgEnd
    lea ax, msg
    sub cx, ax      ;cx = string length
    mov ax, 1301h   ;ah = 13h, al = 1
    mov bl, 7       ;gray
    push cs
    pop es
    lea bp, msg
    int 10h         ;write string

printed:
    pop es
    pop si          ;re-store registers...
    pop dx
    pop cx
    pop bx
    pop ax
endm PRINT
;********************************************************************

;set cursor position
;Input: row, column, page number
macro setCursor row, column, page
    push ax     ;save ax
    push bx     ;save bx
    push dx     ;save dx
    mov dh, row
    mov dl, column
    mov bh, page
    mov ah, 2   ;set cursor position
    int 10h
    pop dx      ;return dx to origin
    pop bx      ;return bx to origin
    pop ax      ;return ax to origin
endm setCursor
;********************************************************************

;Draw player: (+clear last position)

macro DrawPlayer
    local Draw, a, d, checkEnter, Dly, NotPause, check, move, clear, endShoot, NoLives, end, Damage, PlayerDead, PlayerCheckInternal, PlayerCheck, PrePlayerCheck, clearDeadPlayer
    push si
    push ax
    push bx
    push dx

    xor dh, dh
    mov si, playerLoc   ;si <- playerLoc
    cmp si, 60483       ;60481 is the first cell in line 190 and the player is 3 pixels to each side
    jnz check
    mov dh, 1           ;can't move left
    jmp move
check:
    cmp si, 60796       ;60800 is the last cell in line 190 and the player is 3 pixels to each side
    jnz move
    mov dh, 2           ;can't move right
move:
    xor al, al
    mov ah, 1
    int 16h
    jz Draw             ;no key pressed
    mov ah, 7
    int 21h             ;get the key
    cmp al, 27          ;Esc key
    jnz checkEnter      ;press Esc to exit
    mov cl, al
    jmp end             ;for popping all the pushed registers
checkEnter:
    cmp al, 13          ;Enter key
    jnz NotPause        ;press Enter to pause
    call Pause
NotPause:
    cmp playerBulletLoc, 0
    jnz a               ;bullet not ready
    cmp al, 20h         ;SPACE key
    jnz a
    call playerShoot    ;press space to shoot
    mov playerShot, 1   ;player has shot
    mov ax, ShotSoundConstant
    mov ShotSound, ax
    mov al, ShotSoundCounterConstant
    mov ShotSoundCounter, al
    jmp Draw
a:  cmp al, 'a'         ;press 'a' to move left
    jnz d
    cmp dh, 1
    jz d                ;can't move left?
    call clearPlayer
    sub si, playerSpeed ;move to the left
    mov playerLoc, si   ;update location
    jmp Draw
d:  cmp al, 'd'         ;press 'd' to move right
    jnz Draw
    cmp dh, 2
    jz Draw             ;can't move right?
    call clearPlayer
    add si, playerSpeed ;move to the right
    mov playerLoc, si   ;update location

Draw:
    mov al, Orange          ;orange color
    cmp enemyAnimation, 1
    jz JetAnimation
    mov al, Black
    mov es:[si], al         ;animate jet
    mov al, Orange
    jmp JetAnimation2
JetAnimation:
    mov es:[si], al
JetAnimation2:
    mov es:[si-320], al
    mov es:[si-320-1], al
    mov es:[si-320+1], al   ;jet
    mov al, 07h             ;grey color
    mov es:[si-640], al
    mov es:[si-640-1], al
    mov es:[si-640-2], al
    mov es:[si-640+1], al
    mov es:[si-640+2], al   ;body bottom row
    mov es:[si-960], al
    mov es:[si-960-1], al
    mov es:[si-960-2], al
    mov es:[si-960-3], al
    mov es:[si-960+1], al
    mov es:[si-960+2], al
    mov es:[si-960+3], al   ;body longest row
    mov al, 73              ;sky color
    mov es:[si-1280], al
    mov es:[si-1280-1], al
    mov es:[si-1280+1], al  ;window
    mov al, 07h             ;grey color
    mov es:[si-1280-2], al
    mov es:[si-1280+2], al  ;body with window row
    mov es:[si-1600], al
    mov es:[si-1600-1], al
    mov es:[si-1600+1], al  ;body top row
    mov al, gun             ;gun powerup
    mov es:[si-1920], al    ;gun

    cmp playerShot, 1
    jnz Damage              ;player shot?
    xor al, al              ;black color
    mov si, playerBulletLoc
    mov es:[si], al         ;clear top
    mov es:[si+320], al     ;clear middle
    mov es:[si+640], al     ;clear bottom
    sub si, bulletSpeed
    cmp si, 9600            ;row 30 - end of bullets
    jc endShoot             ;if shoot out of the space delete it
    mov playerBulletLoc, si
    mov al, gun
    mov es:[si], al         ;draw top
    mov es:[si+320], al     ;draw middle
    mov es:[si+640], al     ;draw bottom

    mov al, ShotSoundCounter
    cmp al, 0
    jz Damage
    dec al
    mov ShotSoundCounter, al
    call openSpeaker    ;shot sound
    mov ax, ShotSound
    ;add ax, ShotSoundInc
    shr ax, 1       ;sound effect
    mov ShotSound, ax
    mov note, ax
    call playSound

    jmp Damage
endShoot:
    mov playerBulletLoc, 0
    mov playerShot, 0       ;player's shot stopped

Damage:
    mov bx, 6
PrePlayerCheck:
    mov si, playerLoc
    sub si, 3               ;player's bottom left corner
    mov ah, 7
PlayerCheck:
    mov al, 7
PlayerCheckInternal:
    cmp si, enemyBulletLoc[bx-2]
    jz PlayerDead
;    cmp si, enemyBulletLoc[bx]
;    jz PlayerDead
;    cmp si, enemyBulletLoc[bx]
;    jz PlayerDead
    sub si, 320             ;up 1 pixel
    dec al
    jnz PlayerCheckInternal
    add si, 2241            ;down 7 pixels and right 1 pixel
    dec ah
    jnz PlayerCheck
    sub bx, 2
    jnz PrePlayerCheck
    jmp end
PlayerDead:
    call clearPlayer
    xor al, al          ;black color
    mov si, playerLoc
clearDeadPlayer:
    mov es:[si-1920], al    ;gun
    call Delay
    mov es:[si-1600], al
    mov es:[si-1600-1], al
    mov es:[si-1600+1], al  ;body top row
    call Delay
    mov es:[si-1280], al
    mov es:[si-1280-1], al
    mov es:[si-1280+1], al  ;window
    mov es:[si-1280-2], al
    mov es:[si-1280+2], al  ;body with window row
    call Delay
    mov es:[si-960], al
    mov es:[si-960-1], al
    mov es:[si-960-2], al
    mov es:[si-960-3], al
    mov es:[si-960+1], al
    mov es:[si-960+2], al
    mov es:[si-960+3], al   ;body longest row
    call Delay
    mov es:[si-640], al
    mov es:[si-640-1], al
    mov es:[si-640-2], al
    mov es:[si-640+1], al
    mov es:[si-640+2], al   ;body bottom row
    call Delay
    mov es:[si], al
    mov es:[si-320], al
    mov es:[si-320-1], al
    mov es:[si-320+1], al   ;jet
    call Delay

    dec Lives
    mov al, Lives
    mov InGameLives, al     ;update for next level
    jz NoLives
    xor al, al
    cmp al, SpeakerON
    jz noNeedToCloseBefore   ;check if speaker is on when game stoped and turn it off
    call closeSpeaker
noNeedToCloseBefore:
    call Start321
    jmp Loading
NoLives:
    mov Win, 0
    jmp Winner

end:
    pop dx
    pop bx
    pop ax
    pop si
    cmp cl, 27              ;Esc pressed?
    jz PreMainMenu          ;If yes then start the game over...
endm DrawPlayer
;********************************************************************

;Draw enemy:

macro DrawEnemy
    local leftRightLoopNext, preposition, position, preMove, move, draw, right, left, leftRightLoop, left2, end, Enemy1, Enemy2, Enemy3, Enemy4, NextOne, noShoot, checkShoot, drawShoot, IndexOK, preNoShoot, relocation, NextShoot, moveLoop, CheckHit
    push si
    push ax
    push bx
    push cx
    push dx

    xor ch, ch
    mov cl, enemyCount  ;cx = number of enemies
    cmp cl, 0
    jz Winner
    ;mov si, cx
    ;add si, cx          ;in word size array si*2 is needed
    ;sub si, 2           ;arrays start from 0 and I multiplayed by 2
    ;mov ax, enemyLoc[0] ;last enemy
    xor si, si
    mov bx, 320
right:
    mov ax, enemyLoc[si]
    cmp ax, 57280
    jc leftRightLoopNext    ;enemy touched player?
    mov Win, 0          ;you lose...
    jmp Winner
leftRightLoopNext:
    xor dx, dx
    div bx              ;where in the row?
    cmp dx, 316         ;toched right side? (320-4)
    jnz left
    ;add row, 320       ;next row
    mov enemyDir, 0     ;now move left
    jmp preposition
left:
    mov ax, enemyLoc[si]
    ;mov bx, 320
    xor dx, dx
    div bx              ;where in the row?
    cmp dx, 3           ;toched left side?
    jnz leftRightLoop
    ;add row, 320        ;next row
    mov enemyDir, 1     ;now move right
    jmp preposition
leftRightLoop:
    add si, 2
    loop right
    jmp preMove

    ;in each row 20 enemies can feet in (each enemy is 7x7)
    ;than because there are 22 spaces each space will be 8 pixels (320-7*20=180/22=8)
    ;mov ax, cx
    ;mov bl, 30
    ;div bl
    ;mov ch, ah          ;how many rows
    ;xor bx, bx
preposition:
    ;mov al, cl
    ;mul dl
    ;sub ax, 5
    ;mov si, ax
    ;mov ax, row         ;which row
    call clearEnemies   ;clear all enemies
    xor ch, ch
    mov cl, enemyCount  ;cx = number of enemies
    xor si, si
    mov ax, enemyVerticalSpeed
position:
    add enemyLoc[si], ax    ;update location to next row
    add si, 2
    dec cl
    jnz position
    mov enemyMove, 1    ;for not waiting some runs
preMove:
    dec enemyMove       ;move now?
    jnz CheckHit
    xor ch, ch
    mov cl, enemyCount  ;cx = number of enemies
    xor bx, bx
    xor enemyAnimation, 1   ;flip state
    jz  EnemyMakeSound2
EnemyMakeSound1:
    call openSpeaker
    mov ax, EnemySound
    mov note, ax    ;E3
    call playSound
    jmp ClearAfterSound
EnemyMakeSound2:
    call openSpeaker
    mov ax, EnemySound2
    mov note, ax    ;D#3
    call playSound
ClearAfterSound:
    call clearEnemies   ;clear all enemies

move:
    mov ax, enemyHorizontalSpeed
    cmp enemyDir, 1
    jnz left2
    add enemyLoc[bx], ax    ;move right
    jmp draw
left2:
    sub enemyLoc[bx], ax    ;move left

draw:
    mov si, enemyLoc[bx]
    mov ax, enemyType[bx]   ;ax = enemy type
    ;dec ax              ;enemy index starts in 1
    ;push bx
    ;mov dl, 20          ;enemies in a row
    ;div dl              ;al = row number
    ;mov dl, row         ;bh = how many rows of the same enemy
Enemy1:
    cmp al, 1
    jnz Enemy2
    call drawEnemy1     ;draw enemy at that location after it has moved
    ;mov enemyType[bx], 1;make sure it is that type
    jmp NextOne
Enemy2:
    cmp al, 2           ;row
    jnz Enemy3
    call drawEnemy2     ;draw enemy at that location after it has moved
    ;mov enemyType[bx], 2;make sure it is that type
    jmp NextOne
Enemy3:
    ;add dl, dl          ;row*2
    cmp al, 3
    jnz Enemy4
    call drawEnemy3     ;draw enemy at that location after it has moved
    ;mov enemyType[bx], 3;make sure it is that type
    jmp NextOne
Enemy4:
    ;cmp al, row+row+row ;if no enemy has been drawn yet
    ;jnz
    call drawEnemy4     ;draw enemy at that location after it has moved
    ;mov enemyType[bx], 4;make sure it is that type
    ;jmp NextOne
NextOne:
    ;pop bx
    add bx, 2
    loop move
    mov enemyMove, enemyMoveConst

CheckHit:
    HitEnemy

    ;call Random
    ;cmp dx, enemyShootLimit ;below that limit, the enemy won't shoot
    ;jc preNoShoot       ;no new shoot!
    ;mov ax, dx          ;ax = random
    ;xor dx, dx
    ;mov bl, enemiesInLevel
    mov cl, enemiesInLevel
    ;xor bh, bh
    xor ch, ch
    ;div bx              ;dx = enemy index but could be odd and must be even
;    test dx, 1
;    jz IndexOK
;    dec dx              ;make even
;IndexOK:
    ;mov bx, dx
EnemyCheckShoot:
    push cx
    mov dx, cx
    sub dl, enemyCount
    ;sbb dx, 0           ;if borrow
    ;cmp dl, 0           ;enemy index - enemyCount <= 0
    jns NextEnemyCheckToShoot   ;enemy doesn't exist!
    mov bx, cx          ;enemies in level
    add bx, bx          ;doubled for word array
    mov si, enemyLoc[bx]
    xor ch, ch
    mov cl, enemyCount  ;cx = number of enemies
    xor bx, bx
checkShoot:
    mov dx, enemyLoc[bx]
    sub dx, si
    cmp dx, 4800        ;the sapce between two enemies in a column (20*15+20+320*14 = 4800)
    jz NextEnemyCheckToShoot
    add bx, 2
    loop checkShoot
    call Random
    cmp dx, enemyShootLimit ;below that limit, the enemy won't shoot
    jnc EnemyShooting
NextEnemyCheckToShoot:
    pop cx
    loop EnemyCheckShoot
    jmp preNoShoot

EnemyShooting:
    pop cx  ;needed because cx wasn't popped at the end of the loop...
    cmp enemyBulletLoc[4], 0    ;ready to shoot?
    jnz preNoShoot
    call enemyShoot             ;enemy shoot

preNoShoot:
    xor bx, bx
    mov cx, 3               ;max number of bullets
noShoot:
    cmp enemyBulletLoc[bx], 0
    jz NextShoot            ;no bullet
    xor al, al              ;black color
    mov si, enemyBulletLoc[bx]
    mov es:[si-640+1], al
    mov es:[si-640], al
    mov es:[si-640-1], al
    mov es:[si-320], al
    mov es:[si], al         ;clear bullet
    add si, bulletSpeed
    cmp si, 60800           ;row 190 - end of bullet's
    jc drawShoot            ;if shoot out of the space delete it
    mov enemyBulletLoc[bx], 0   ;delete bullet
    cmp bx, 2
    jnz relocation
    mov ax, enemyBulletLoc[bx+2]
    mov enemyBulletLoc[bx], ax
    mov enemyBulletLoc[bx+2], 0 ;delete the 3rd bullet
    jmp NextShoot
relocation:
    cmp bx, 0
    jnz NextShoot
    mov ax, enemyBulletLoc[bx+2]
    mov enemyBulletLoc[bx], ax
    mov ax, enemyBulletLoc[bx+4]
    mov enemyBulletLoc[bx+2], ax
    mov enemyBulletLoc[bx+4], 0 ;delete the 3rd bullet
    jmp NextShoot
drawShoot:
    mov enemyBulletLoc[bx], si
    mov al, White
    mov es:[si-640+1], al
    mov es:[si-640], al
    mov es:[si-640-1], al
    mov es:[si-320], al
    mov es:[si], al         ;draw bullet
NextShoot:
    add bx, 2
    loop noShoot
    jmp end

end:
    pop dx
    pop cx
    pop bx
    pop ax
    pop si
endm DrawEnemy
;********************************************************************

;Check if player hit an enemy:

macro HitEnemy
    local AfterShot, NoLaser, EnemyCheck, EnemyDead, EnemyDeadLoop, HitEnemyEnd, EnemyCheckInternal, DeleteEnemy, DeleteEnemyInternal
    push si
    push ax
    push bx
    push cx

    xor bx, bx
    mov cl, enemyCount
    xor ch, ch
preEnemyCheck:
    mov ah, 7
    mov si, enemyLoc[bx]
    add si, 957         ;enemy's bottom left corner
EnemyCheck:
    mov al, 7
EnemyCheckInternal:
    cmp si, playerBulletLoc
    jz EnemyDead
    ;cmp si, playerBulletLoc
    ;jz EnemyDead
    ;cmp si, playerBulletLoc
    ;jz EnemyDead
    sub si, 320         ;up 1 pixel
    dec al
    jnz EnemyCheckInternal
    add si, 1921        ;down 6 pixels and right 1 pixel
    dec ah
    jnz EnemyCheck
    jmp EnemyNotDead
EnemyDead:
    push bx
    mov si, enemyLoc[bx]
    mov ax, enemyType[bx]
    add Scores, ax      ;add scores for the type of enemy
    call PrintScores
    xor bl, bl          ;black color
    add si, 960-3       ;bottom left corner of enemy's box
    mov ah, 7
DeleteEnemy:
    mov al, 7
DeleteEnemyInternal:
    mov es:[si], bl
    sub si, 320         ;up 1 pixel
    dec al
    jnz DeleteEnemyInternal
    add si, 2241        ;down 7 pixels and right 1 pixel
    dec ah
    jnz DeleteEnemy
    pop bx
    push bx             ;save bx again
    mov si, enemyLoc[bx]
    call Boom           ;enemy -> KABOOM!
    ;lea ax, enemyLocEnd
    ;lea si, enemyLoc
    ;sub ax, si          ;how much enemies
    ;inc ax              ;for how much to do enemyDeadLoop
    ;sub ax, cx          ;how much enemies after that enemy in the array
    mov ax, cx          ;how much to do enemyDeadLoop is how much left
EnemyDeadLoop:
    mov si, enemyLoc[bx+2]
    mov enemyLoc[bx], si    ;sort enemy locations
    mov si, enemyType[bx+2]
    mov enemyType[bx], si   ;sort enemy types
    add bx, 2
    dec ax
    jnz EnemyDeadLoop
    dec enemyCount      ;-1 enemy
    ;mov enemyDead, 1
    cmp gun, Red        ;Laser?
    jnz NoLaser
    dec Laser
    jnz AfterShot
NoLaser:
    mov si, playerBulletLoc
    xor al, al
    mov es:[si], al
    mov es:[si+320], al
    mov es:[si+640], al ;clear bullet
    mov playerBulletLoc, 0
    mov playerShot, 0   ;player's shot stopped
    mov Laser, LaserConst
AfterShot:
    pop bx
    sub bx, 2           ;because of the sorting no need to add 2 in EnemyNotDead!
EnemyNotDead:
    add bx, 2
    loop preEnemyCheck

HitEnemyEnd:
    pop cx
    pop bx
    pop ax
    pop si
endm HitEnemy
;********************************************************************

proc Boom
    push si
    push ax
    push dx

    ;mov si, enemyLoc[bx]
    mov BoomLoc, si
    mov al, White           ;boom color
    mov es:[si-960-3], al
    mov es:[si-960], al
    mov es:[si-960+3], al   ;top row
    mov es:[si-640-2], al
    mov es:[si-640], al
    mov es:[si-640+2], al   ;second row
    mov es:[si-3], al
    mov es:[si-2], al
    mov es:[si+2], al
    mov es:[si+3], al       ;middle row
    mov es:[si+640-2], al
    mov es:[si+640], al
    mov es:[si+640+2], al   ;forth row
    mov es:[si+960-3], al
    mov es:[si+960], al
    mov es:[si+960+3], al   ;bottom row

    call random     ;refresh and add randomness!

    pop dx
    pop ax
    pop si
    ret
endp Boom
;********************************************************************

;program area (Main):

START:

initialization:
    mov ax, @data
    mov ds, ax          ;ds = data segment
    mov ax, 0A000h      ;screen memory
    mov es, ax          ;screen memory segment
    mov si, 80
    mov ax, 13h         ;ah = 0 al = 13
    int 10h             ;ah = 0: set video mode al = 13: graphical mode. 40x25. 256 colors. 320x200 pixels. 1 page.
    mov ah, 3
    int 10h             ;get cursor position and size
    mov cursorPage, bh  ;save Page
    mov ah, 1
    mov ch, 00100000b
    int 10h             ;ah = 1, ch bit 5 = 1 -> hide cursor (_)
    ;mov ax, 1003h       ;toggle intensity/blinking
    ;xor bx, bx          ;bx = 0 -> enable intensive colors
    ;int 10h
    mov ah, 3
    mov al, 5           ;Set typematic rate and delay
    xor bl, bl          ;Typematic Rate in Characters per Second
    xor bh, bh          ;Delay Value in Milliseconds
    int 16h             ;Sets the typematic rate and delay of the keyboard
    lea dx, FileName
    call GetHandle      ;open data file for scoreboard, DX point to file name

PreMainMenu:
    call clearEntireScreen
    mov Win, 1      ;Start as a winner!
    mov FirstRun, 1 ;it is the first run!
    mov al, InGameLives ;keep lives updated
    mov Lives, al
    push cx         ;save cx
    xor ah, ah
    mov al, 1
    mov bl, 20      ;enemies per row
Levelcheck:
    cmp Level, al
    jnz NotThatLevel
    cmp al, 255     ;last level
    jz BigWin
    cmp al, 9
    jc NotMaxEnemy
    mov al, 9       ;9 rows of enemy is the maximum!
NotMaxEnemy:
    mul bl
    mov enemyCount, al
    mov enemiesInLevel, al
    push ax         ;save ax

enemyTypeCreate:
    xor ah, ah
    mov al, enemiesInLevel  ;total number of enemies
    mov ch, 20      ;enemies in row
    div ch          ;al = number of rows
    mov ch, al      ;ch = number of rows
    xor ax, ax
    xor bx, bx
enemyTypeCreateLoop:
    mov cl, 20
    inc ax          ;start at 1
enemyTypeCreateInternal:
    mov enemyType[bx], ax
    add bx, 2
    dec cl
    jnz enemyTypeCreateInternal
    dec ch
    jnz enemyTypeCreateLoop

    ;push sp
    push bp
enemiesFirstLocations:
    mov bl, 15          ;space between two centers of enemies
    mov bh, 20          ;enemies in row
    mov si, 320         ;pixels in row
    mov bp, 14          ;spaces between rows
    xor ch, ch
    mov cl, enemiesInLevel  ;cx = number of enemies
    ;enemy centere position = 26, 41, 56... (15*cx+8+4-1+20*row+row*320*14) in reverse order!
    xor ah, ah
    xor di, di
putLoc:
    xor dx, dx
    ;push ax
    mov ax, cx          ;ax = enemy index
    dec ax              ;indexes starts from 0
    div bh              ;ax = row number
    xor ah, ah
    push ax
    ;mov bp, dx          ;bp = position in row
    mov dx, 20
    mul dl              ;ax = how much to add with the row
    pop dx              ;dx = row number
    xchg ax, dx         ;dx = how much to add with the row, ax = row number
    push dx
    xor dx, dx
    mul si              ;ax = start of row
    xor dx, dx
    ;push cx
    ;mov cx, 14          ;spaces between rows
    mul bp              ;ax = start of row but with space
    ;pop cx
    pop dx              ;dx = how much to add with the row
    add ax, dx          ;ax + 20*row for alignment
    ;add ax, bp          ;ax = position in row
    mov dx, ax
    mov al, bl
    mul cl              ;ax = al*cl = 15*cx
    add ax, 11          ;8+4-1 = 11
    add ax, dx
    add ax, 16000       ;start from row 50
    mov enemyLoc[di], ax
    ;xor ah, ah
    add di, 2
    loop putLoc
    pop bp
    ;pop sp

    pop ax          ;ax first
    pop cx          ;then cx
    cmp cl, 27
    jz MainMenu     ;Main Menu through escape
    cmp al, 20      ;enemies in the first level
    jz MainMenu
    jmp Loading
NotThatLevel:
    inc al
    jmp Levelcheck

;JustBeforeMainMenu:
    ;call clearEntireScreen
MainMenu:
    setCursor 0, 0, cursorPage
    PRINT "W&S keys - navigate, Enter - select"
    setCursor 21, 0, cursorPage
    PRINT "During gameplay:"
    setCursor 22, 0, cursorPage
    PRINT "A&S - move, SPACE - shoot"
    setCursor 23, 0, cursorPage
    PRINT "Press Enter to Pause."
    setCursor 24, 0, cursorPage
    PRINT "Press Esc to return to the menu."
    setCursor 8, 14, cursorPage
    PRINT "SPACE SHOOTER"
    cmp navigation, 0
    jz StartGameSelected
StartGame:
    setCursor 10, 15, cursorPage
    PRINT "START  GAME"
    jmp AfterStartGame
StartGameSelected:
    setCursor 10, 13, cursorPage
    PRINT "[ START  GAME ]"
AfterStartGame:
    cmp navigation, 1
    jz ScoreBoardSelected
ScoreBoard:
    setCursor 12, 15, cursorPage
    PRINT "SCORE BOARD"
    jmp AfterScoreBoard
ScoreBoardSelected:
    setCursor 12, 13, cursorPage
    PRINT "[ SCORE BOARD ]"
AfterScoreBoard:
    cmp navigation, maxNav
    jz QuitGameSelected
QuitGame:
    setCursor 14, 16, cursorPage
    PRINT "QUIT GAME"
    jmp AfterQuitGame
QuitGameSelected:
    setCursor 14, 14, cursorPage
    PRINT "[ QUIT GAME ]"
AfterQuitGame:
    mov FirstTime, 1    ;reset time display
    mov MinutesCanPass, 0
    mov HoursCanPass, 0
    mov MinutesHEX, 0
    mov HoursHEX, 0
    mov Seconds[0], '0'
    mov Seconds[1], '0'
    mov Minutes[0], '0'
    mov Minutes[1], '0'
    mov Hours[0], '0'
    mov Hours[1], '0'
    mov Scores, 0   ;reset scores
    mov ScoresString[0], '0'
    mov ScoresString[1], '0'
    mov ScoresString[2], '0'
    mov ScoresString[3], '0'
    mov ScoresString[4], '0'    ;reset scores
    mov Lives, StartLives       ;start with 3 lives
    mov al, Lives      ;update in game lives
    mov InGameLives, al
    call Stars
    xor al, al
    mov ah, 1
    int 16h
    jz MainMenuEnd      ;no key pressed
    mov ah, 7
    int 21h             ;get the key
    cmp al, 0Dh         ;Enter
    jnz w
    cmp navigation, 0
    jz Loading
    cmp navigation, 1
    jz  WatchScoreBoard
    cmp navigation, maxNav
    jz endGame
    jmp MainMenuEnd
w:
    cmp al, 'w'
    jnz s
    cmp navigation, 0
    jz MainMenuEnd
    dec navigation
    jmp MainMenuEnd
s:
    cmp al, 's'
    jnz MainMenuEnd
    cmp navigation, maxNav
    jz MainMenuEnd
    inc navigation
    ;jmp MainMenuEnd
MainMenuEnd:
    call Delay
    ;call Delay60FPS
    setCursor 10, 15, cursorPage
    call clearText      ;clear 'start game'
    setCursor 12, 15, cursorPage
    call clearText      ;clear 'score board'
    setCursor 14, 16, cursorPage
    call clearText      ;clear 'quit game'
    jmp MainMenu

Loading:
    setCursor 25, 30, cursorPage
    PRINT "LOADING..."

    mov playerBulletLoc, 0
    mov playerLoc, 60640
    mov playerShot, 0
    mov enemyBulletLoc[0], 0
    mov enemyBulletLoc[2], 0
    mov enemyBulletLoc[4], 0

    call clearEntireScreen
    setCursor 0, 0, cursorPage
    PRINT 'Scores: '    ;print scores to the screen for the first time
    push es
    mov bh, cursorPage  ;page number
    mov ah, 3
    int 10h             ;get cursor position for dh and dl to match the position
    mov ax, 1301h       ;ah = 13h, al = 1
    mov cx, 5           ;number of characters to write
    mov bl, 7           ;attribute
    push ds
    pop es
    lea bp, ScoresString
    int 10h             ;write character with attribute
    pop es

    setCursor 1, 0, cursorPage
    PRINT 'Level: '
    call printLevelNum

overagain:
    cmp FirstRun, 0
    jz GamePhase
    call Start321       ;Delay at the start of each level...
    mov FirstRun, 0     ;never come back in this level!
GamePhase:
    DrawPlayer
    DrawEnemy
    ;call stars
    call Bonus
    call checkBoom
    call PrintLives
    call gunTime
    call TimeDisplay
    call Delay
    ;call Delay60FPS
    xor al, al
    cmp al, SpeakerON
    jz noNeedToClose
    call closeSpeaker
noNeedToClose:
    jmp overagain

Winner:
    xor al, al
    cmp al, SpeakerON
    jz noNeedToCloseAtEnd   ;check if speaker is on when game stoped and turn it off
    call closeSpeaker
noNeedToCloseAtEnd:
    cmp Win, 1
    jz YouWin
    call YouLose
    jmp PreMainMenu
YouWin:
    setCursor 12, 14, cursorPage
    PRINT 'You Won!'
    ;call printLevelNum
    setCursor 14, 10, cursorPage
    PRINT 'Loading Next Level...'
    inc Level
    call BigDelay
    setCursor 12, 14, cursorPage
    call clearText      ;clear 'You Won Level '
    setCursor 14, 10, cursorPage
    call clearText      ;clear 'Loading Next Level...'
    ;call clearEntireScreen
    jmp PreMainMenu

BigWin:
    setCursor 12, 13, cursorPage
    PRINT 'Congratulations!'
    setCursor 14, 16, cursorPage
    PRINT 'You  Won!'
    call BigDelay
    setCursor 12, 14, cursorPage
    call clearText      ;clear 'Congratulations!'
    setCursor 14, 10, cursorPage
    call clearText      ;clear 'You Won!'
    mov Level, 1
    jmp PreMainMenu

endGame:
call CloseFile      ;close data (end use)
mov ax, 03h         ;ah = 0 al = 3
int 10h             ;ah = 0: set video mode al = 3: text mode. 80x25. 16 colors. 8 pages
mov ah, 4Ch         ;return control to the operating system!
int 21h

FileError:
    setCursor 0, 0, cursorPage
    PRINT 'Error'
    mov ah, 7
    int 21h     ;wait for keypress
    jmp endGame

;********************************************************************

;procedures area:

;********************************************************************

Delay:
    push cx
    xor cx, cx
D:  loop D
    pop cx
ret
;********************************************************************

BigDelay:
    push ax
    push cx
    mov al, BigDelayTime
    xor cx, cx
BD: loop BD
    dec al
    jnz BD
    pop cx
    pop ax
ret
;********************************************************************

SecondDelay:
    push ax
    push cx
    push es

    mov ax, 40h
    mov es, ax
    mov cx, 18      ;18*0.55=~1sec
clocking:
    mov ax, clock
Ticking:
    cmp ax, clock
    jz Ticking
    loop clocking

    pop es
    pop cx
    pop ax
ret
;********************************************************************

Delay60FPS:
    push ax
    push cx
    push dx

    xor cx, cx
    mov dx, 16667   ;0.016667s
    mov ah, 86h
    int 15h     ;wait CX:DX microseconds

    pop dx
    pop cx
    pop ax
ret
;********************************************************************

Stars:
    push ax
    push bx
    push cx
    push dx
    push si

    dec starSpeed
    jnz endOfStars
    mov cl, starCount   ;cx = number of stars
    xor ch, ch
    cmp cx, 0
    jz create
    xor bx, bx
    mov ax, 000Fh       ;ah = black (for deleting) al = white
fallingStars:
    mov si, starLoc[bx] ;first star location
    cmp si, 64000       ;off screen?
    ;cmp si, StarEnd
    jc fall
    push cx             ;save cx
sorting:
    mov si, starLoc[bx+2]
    mov starLoc[bx], si ;clear the one before (shift right all the array)
    add bx, 2
    loop sorting
    mov bl, 1
    sub starCount, bl   ;1 star died...
    xor bx, bx
    pop cx              ;restore cx
    dec cx              ;-1 star to go over
    jz create
    jmp fallingStars
fall:
    mov es:[si], ah     ;clear the star
    add si, 320         ;1 row down
    mov es:[si], al     ;star in new place
    mov starLoc[bx], si ;save new place
    add bx, 2
    loop fallingStars

create:
    ;create a new star:
    ;xor ah, ah          ;int 1Ah, ah = 0
    ;int 1Ah             ;CX:DX = number of clock ticks since midnight.
    call Random
    mov ax, dx          ;ax = random
    mov cx, 320         ;divide ax by 320
    xor dx, dx          ;no dx (dx:ax/cx) in division!!!
    div cx              ;then the reminder will be from 0 to 319 (like the screen width)
    mov al, dl          ;al = reminder
    mov ah, 2           ;multiply al by 2
    mul ah              ;al = cell (only the positive place is the char to display)
    ;add ax, StarStart
    mov si, ax          ;si = cell for star
    mov al, White       ;A star color
    mov es:[si], al     ;place on top of the screen
    mov al, starCount
    mov bl, 2
    mul bl              ;ax = where to put the star location
    mov bx, ax
    mov starLoc[bx], si   ;where is the star?
    mov bx, 1
    add starCount, bl   ;inc starCount by 1
    ;A new star was born!
    mov starSpeed, starSpeedConst
endOfStars:
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
ret
;********************************************************************

Random:
;random = (a*rand+b) mod m = (31*rand+13)%19683

    push ax
    push bx

    mov ax, rand
first_time:
    jmp jump
jump:
    push si
    push di

    mov si, first_time  ;si = first_time address
    mov di, no_first    ;di = no_first address
    sub di, jump        ;distance from jump to no_First
    mov ax, di          ;al = distance
    mov ah, 0EBh        ;jmp opcode
    xchg al, ah         ;al = EBh (jmp opcode), ah = distance
    mov cs:[si], ax     ;jmp jump -> jmp no_first

    mov ax, 43962       ;initial value

    pop di
    pop si

no_first:
    mov bx, 31
    mul bx              ;rand*31
    add ax, 13	        ;+13
    mov bx, 19683
    div bx              ;%19683
    mov rand, dx        ;save
    ;random number in dx

    pop bx
    pop ax
ret
;********************************************************************

checkBoom:
    cmp BoomLoc, 0
    jz EndofBoom
    call openSpeaker
    mov ax, BoomSound
    mov note, ax    ;C#2
    call playSound
    dec BoomTime
    jnz EndofBoom
    mov si, BoomLoc
    xor al, al              ;black color
    mov es:[si-960-3], al
    mov es:[si-960], al
    mov es:[si-960+3], al   ;top row
    mov es:[si-640-2], al
    mov es:[si-640], al
    mov es:[si-640+2], al   ;second row
    mov es:[si-3], al
    mov es:[si-2], al
    mov es:[si+2], al
    mov es:[si+3], al       ;middle row
    mov es:[si+640-2], al
    mov es:[si+640], al
    mov es:[si+640+2], al   ;forth row
    mov es:[si+960-3], al
    mov es:[si+960], al
    mov es:[si+960+3], al   ;bottom row
    mov BoomTime, BoomTimeConst
    mov BoomLoc, 0
EndofBoom:
ret
;********************************************************************

clearPlayer:
    push ax

    xor al, al              ;black color
    mov es:[si], al
    mov es:[si-320], al
    mov es:[si-320-1], al
    mov es:[si-320+1], al   ;jet
    mov es:[si-640], al
    mov es:[si-640-1], al
    mov es:[si-640-2], al
    mov es:[si-640+1], al
    mov es:[si-640+2], al   ;body bottom row
    mov es:[si-960], al
    mov es:[si-960-1], al
    mov es:[si-960-2], al
    mov es:[si-960-3], al
    mov es:[si-960+1], al
    mov es:[si-960+2], al
    mov es:[si-960+3], al   ;body longest row
    mov es:[si-1280], al
    mov es:[si-1280-1], al
    mov es:[si-1280+1], al  ;window
    mov es:[si-1280-2], al
    mov es:[si-1280+2], al  ;body with window row
    mov es:[si-1600], al
    mov es:[si-1600-1], al
    mov es:[si-1600+1], al  ;body top row
    mov es:[si-1920], al    ;gun

    pop ax
ret
;********************************************************************

playerShoot:
    push ax

    mov al, gun             ;gun powerup
yellowType:
    cmp al, Yellow
    jnz redType
    mov es:[si-2240], al
    mov es:[si-2560], al
    mov es:[si-2880], al    ;bullet
    jmp PbulletLoc
redType:
    cmp al, Red
    jnz blueType
    mov es:[si-2240], al
    mov es:[si-2560], al
    mov es:[si-2880], al    ;bullet
    jmp PbulletLoc
blueType:
    cmp al, Blue
    jnz grayType
    mov es:[si-2240], al
    mov es:[si-2560], al
    mov es:[si-2880], al    ;bullet
    jmp PbulletLoc
grayType:
    mov es:[si-2240], al
    mov es:[si-2560], al
    mov es:[si-2880], al    ;bullet
    ;jmp PbulletLoc
    PbulletLoc:
    mov playerBulletLoc, si ;player's bottom
    sub playerBulletLoc, 2880   ;bullet location = bullet's top

    pop ax
ret
;********************************************************************

gunTime:
    cmp gun, Gray
    jz EndGunTime
    dec PowerupTime
    jnz EndGunTime
    mov gun, Gray
    mov PowerupTime, PowerupTimeConst
EndGunTime:
ret
;********************************************************************
clearEnemies:
    push ax
    push bx
    push cx
    push si

    xor al, al              ;black color
    xor bx, bx
    mov bl, enemyCount      ;bl = number of enemies
    add bx, bx              ;bx = bl*2
    sub bx, 2
    mov cx, enemyLoc[0]
    sub cx, enemyLoc[bx]    ;enemies*2-2
    add cx, 1926            ;for +top and bottom rows
    mov si, enemyLoc[bx]
    sub si, 963             ;to clear both top and bottom rows
rowing:
    mov es:[si], al
    inc si
    dec cx
    jnz rowing
    ;add si, 320             ;go back to start but in next line
    ;dec bl
    ;jnz columning

    pop si
    pop cx
    pop bx
    pop ax
ret
;********************************************************************

drawEnemy1:
    push ax

;    mov al, 1               ;enemy 1
;    call PHitE
;    cmp enemyDead, 1
;    jz drawEnemy1End

    mov al, White           ;white color
    mov es:[si-960-2], al
    mov es:[si-960-1], al
    mov es:[si-960], al
    mov es:[si-960+1], al
    mov es:[si-960+2], al   ;top row
    mov es:[si-640-2], al
    mov es:[si-640], al
    mov es:[si-640+2], al   ;eye's row
    mov es:[si-320-2], al
    mov es:[si-320-1], al
    mov es:[si-320], al
    mov es:[si-320+1], al
    mov es:[si-320+2], al
    cmp enemyAnimation, 1
    jz enemy1pos
    mov es:[si-320+3], al
    mov es:[si-320-3], al
    jmp enemy1Afterpos
enemy1pos:
    mov es:[si-640+3], al
    mov es:[si-640-3], al    ;hand's row
enemy1Afterpos:
    mov es:[si-2], al
    mov es:[si-1], al
    mov es:[si], al
    mov es:[si+1], al
    mov es:[si+2], al       ;bottom row
    mov es:[si+320-1], al
    mov es:[si+320+1], al   ;legs
    mov al, Green           ;green color
    mov es:[si-640-1], al
    mov es:[si-640+1], al   ;eyes

drawEnemy1End:
    ;mov enemyDead, 0
    pop ax
ret
;********************************************************************

drawEnemy2:
    push ax

;    mov al, 2               ;enemy 2
;    call PHitE
;    cmp enemyDead, 1
;    jz drawEnemy2End

    mov al, White           ;white color
    mov es:[si-960], al     ;tip top
    mov es:[si-640-2], al
    mov es:[si-640-1], al
    mov es:[si-640], al
    mov es:[si-640+1], al
    mov es:[si-640+2], al   ;head row
    mov es:[si-320-2], al
    mov es:[si-320], al
    mov es:[si-320+2], al   ;eye's row
    mov es:[si-2], al
    mov es:[si-1], al
    mov es:[si+1], al
    mov es:[si+2], al       ;mouth's row
    mov es:[si+320-1], al
    mov es:[si+320], al
    mov es:[si+320+1], al
    mov es:[si+640], al
    cmp enemyAnimation, 1
    jz enemy2pos
    mov es:[si+960+1], al
    mov es:[si+960-1], al
    mov es:[si+640+2], al
    mov es:[si+640-2], al
    jmp enemy2Afterpos
enemy2pos:
    mov es:[si+640+1], al
    mov es:[si+640-1], al
    mov es:[si+320+2], al
    mov es:[si+320-2], al   ;legs
enemy2Afterpos:
    mov al, Green           ;green color
    mov es:[si-320-1], al
    mov es:[si-320+1], al   ;eyes
    mov es:[si], al         ;mouth

drawEnemy2End:
    ;mov enemyDead, 0
    pop ax
ret
;********************************************************************

drawEnemy3:
    push ax

;    mov al, 3               ;enemy 3
;    call PHitE
;    cmp enemyDead, 1
;    jz drawEnemy3End

    mov al, White           ;white color
    mov es:[si-960], al     ;tip top
    mov es:[si-640-1], al
    mov es:[si-640], al
    mov es:[si-640+1], al   ;head
    mov es:[si-320-2], al
    mov es:[si-320], al
    mov es:[si-320+2], al   ;eye's row
    mov es:[si-2], al
    mov es:[si-1], al
    mov es:[si], al
    mov es:[si+1], al
    mov es:[si+2], al       ;body row
    mov es:[si+320-1], al
    mov es:[si+320], al
    mov es:[si+320+1], al   ;bottom row
    mov es:[si+640], al
    cmp enemyAnimation, 1
    jz enemy3pos
    mov es:[si+960+1], al
    mov es:[si+960-1], al
    jmp enemy3Afterpos
enemy3pos:
    mov es:[si+960], al     ;legs
enemy3Afterpos:
    mov al, Green           ;green color
    mov es:[si-320-1], al
    mov es:[si-320+1], al   ;eyes

drawEnemy3End:
    ;mov enemyDead, 0
    pop ax
ret
;********************************************************************

drawEnemy4:
    push ax

;    mov al, 4               ;enemy 4
;    call PHitE
;    cmp enemyDead, 1
;    jz drawEnemy4End

    mov al, White           ;white color
    mov es:[si-960-3], al
    mov es:[si-960-2], al
    mov es:[si-960-1], al
    mov es:[si-960], al
    mov es:[si-960+1], al
    mov es:[si-960+2], al
    mov es:[si-960+3], al   ;top row
    mov es:[si-640-3], al
    mov es:[si-640-1], al
    mov es:[si-640], al
    mov es:[si-640+1], al
    mov es:[si-640+3], al   ;eye's row
    mov es:[si-320-3], al
    mov es:[si-320-2], al
    mov es:[si-320+2], al
    mov es:[si-320+3], al   ;mouth's row
    mov es:[si-3], al
    mov es:[si-2], al
    mov es:[si-1], al
    mov es:[si], al
    mov es:[si+1], al
    mov es:[si+2], al
    mov es:[si+3], al       ;bottom row
    mov es:[si+320-2], al
    mov es:[si+320+2], al
    mov es:[si+960-2], al
    mov es:[si+960+2], al
    cmp enemyAnimation, 1
    jz enemy4pos
    mov es:[si+640-3], al
    mov es:[si+640+3], al
    jmp enemy4Afterpos
enemy4pos:
    mov es:[si+640-2], al
    mov es:[si+640+2], al   ;legs
enemy4Afterpos:
    mov al, Green           ;green color
    mov es:[si-640-2], al
    mov es:[si-640+2], al   ;eyes
    mov es:[si-320-1], al
    mov es:[si-320], al
    mov es:[si-320+1], al   ;mouth

drawEnemy4End:
    ;mov enemyDead, 0
    pop ax
ret
;********************************************************************

enemyShoot:
    push ax

    mov al, White           ;White color
    mov es:[si+1280+1], al
    mov es:[si+1280], al
    mov es:[si+1280-1], al
    mov es:[si+1600], al
    mov es:[si+1920], al    ;draw bullet
firstBullet:
    cmp enemyBulletLoc[0], 0
    jnz secondBullet
    mov enemyBulletLoc[0], si
    add enemyBulletLoc[0], 1920;bullet location = bullet's top
    jmp enemyShootEnd
secondBullet:
    cmp enemyBulletLoc[2], 0
    jnz thirdBullet
    mov enemyBulletLoc[2], si
    add enemyBulletLoc[2], 1920;bullet location = bullet's top
    jmp enemyShootEnd
thirdBullet:
    mov enemyBulletLoc[4], si
    add enemyBulletLoc[4], 1920;bullet location = bullet's top

enemyShootEnd:
    pop ax
ret
;********************************************************************

Bonus:
    push ax
    push dx
    push si

    cmp BonusExist, 1
    jz NoNewBonus           ;never 2 bonuses at the same time
    call random
    cmp dx, BonusLimit
    jc BonusEnd             ;bonus chance
    mov BonusExist, 1
    mov BonusLoc, BonusLocConstant  ;reset position
NoNewBonus:
    call ClearBonus
    mov ax, BonusHorizontalSpeed
    sub BonusLoc, ax        ;move bonus
    mov si, BonusLoc
drawBonus:
    mov al, Red             ;Red color
    mov es:[si-960], al     ;Top row
    mov es:[si-640-1], al
    mov es:[si-640], al
    mov es:[si-640+1], al   ;Second row
    mov es:[si-320-2], al
    mov es:[si-320-1], al
    mov es:[si-320], al
    mov es:[si-320+1], al
    mov es:[si-320+2], al   ;Third row
    mov es:[si], al
    mov es:[si-2], al
    mov es:[si+2], al
    mov es:[si-1], al
    mov es:[si+1], al
    mov es:[si-3], al
    mov es:[si+3], al       ;Windows row
    mov es:[si+320-3], al
    mov es:[si+320-2], al
    mov es:[si+320-1], al
    mov es:[si+320], al
    mov es:[si+320+1], al
    mov es:[si+320+2], al
    mov es:[si+320+3], al   ;Almost Bottom
    mov es:[si+640-1], al
    mov es:[si+640], al
    mov es:[si+640+1], al   ;Bottom row

    mov al, Green           ;Green color
    cmp enemyAnimation, 1
    jz Bonuspos
    mov es:[si-1], al
    mov es:[si+1], al
    mov es:[si-3], al
    mov es:[si+3], al       ;Windows
    jmp PreBonusCheck
Bonuspos:
    mov es:[si], al
    mov es:[si-2], al
    mov es:[si+2], al       ;Windows

PreBonusCheck:
    mov si, BonusLoc
    add si, 957         ;Bonus' bottom left corner
    mov ah, 7           ;7x7 Bonus (and other enemys) square
BonusCheck:
    mov al, 7
BonusCheckInternal:
    cmp si, playerBulletLoc
    jz BonusDead
    sub si, 320         ;up 1 pixel
    dec al
    jnz BonusCheckInternal
    add si, 2241        ;down 7 pixels and right 1 pixel
    dec ah
    jnz BonusCheck
    jmp BonusNotDead

BonusDead:
    call ClearBonus
    call Boom
    mov ax, 10
    add Scores, ax      ;Bonus gives 10 points
    call PrintScores    ;Update score
    mov BonusExist, 0
    jmp BonusEnd

BonusNotDead:
    cmp enemyAnimation, 1
    jz BonusMakeSound2
    call openSpeaker
    mov ax, BonusSound
    mov note, ax    ;E4
    call playSound
    jmp AfterBonusSound
BonusMakeSound2:
    call openSpeaker
    mov ax, BonusSound2
    mov note, ax    ;C4
    call playSound
AfterBonusSound:
    mov si, BonusLoc
    cmp si, 10563       ;row 33 (33*320) left side (+0) +3 pixels
    jc BonusDespawn
    jmp BonusEnd

BonusDespawn:
    call ClearBonus
    mov BonusExist, 0

BonusEnd:
    pop si
    pop dx
    pop ax
ret
;********************************************************************

ClearBonus:
    mov si, BonusLoc
    mov al, Black           ;Black color
    mov es:[si-960], al     ;Top row
    mov es:[si-640-1], al
    mov es:[si-640], al
    mov es:[si-640+1], al   ;Second row
    mov es:[si-320-2], al
    mov es:[si-320-1], al
    mov es:[si-320], al
    mov es:[si-320+1], al
    mov es:[si-320+2], al   ;Third row
    mov es:[si], al
    mov es:[si-2], al
    mov es:[si+2], al
    mov es:[si-1], al
    mov es:[si+1], al
    mov es:[si-3], al
    mov es:[si+3], al       ;Windows row
    mov es:[si+320-3], al
    mov es:[si+320-2], al
    mov es:[si+320-1], al
    mov es:[si+320], al
    mov es:[si+320+1], al
    mov es:[si+320+2], al
    mov es:[si+320+3], al   ;Bottom start
    mov es:[si+640-1], al
    mov es:[si+640], al
    mov es:[si+640+1], al   ;Bottom row
ret
;********************************************************************

;clear 1 line of text
clearText:
    push ax
    push bx
    push cx
    push dx

    mov bh, cursorPage
    mov ah, 3
    int 10h         ;get cursor position
    setCursor dh, 0, cursorPage
    mov ah, 0Eh
    mov al, ' '
    ;mov ch, 25      ;25 lines
preClearLine:
    mov cl, 40      ;40 characters in a line
clearLine:
    int 10h         ;teletype - al to screen and cursor position + 1
    dec cl
    jnz clearLine
    ;dec ch
    ;jnz preClearLine

    pop dx
    pop cx
    pop bx
    pop ax
ret
;********************************************************************

clearEntireScreen:
    push ax
    push cx
    push si

    mov cx, 64000   ;all pixels in screen
    xor al, al      ;black color
    xor si, si
clearPixel:
    mov es:[si], al
    inc si
    loop clearPixel

    pop si
    pop cx
    pop ax
ret
;********************************************************************

Pause:
    push ax

    setCursor 12, 18, cursorPage
    PRINT 'PAUSE'
    setCursor 14, 9, cursorPage
    PRINT 'Press Enter To Continue'
PauseLoop:
    xor al, al
    mov ah, 1
    int 16h
    jz PauseLoop        ;no key pressed
    mov ah, 7
    int 21h             ;get the key
    cmp al, 0Dh         ;Enter
    jnz PauseLoop

PauseStop:
    setCursor 12, 18, cursorPage
    call clearText
    setCursor 14, 9, cursorPage
    call clearText

    pop ax
ret
;********************************************************************

YouLose:
    setCursor 12, 16, cursorPage
    PRINT 'GAME OVER'
    call openSpeaker
    mov ax, 38652
    mov note, ax        ;B0
    call playSound
    ;call BigDelay
    call SecondDelay
    mov ax, 43388       ;A0
    mov note, ax
    call playSound
    call SecondDelay
    ;call SecondDelay
    call closeSpeaker

    mov al, 1
    mov Level, al       ;reset levels

    call clearEntireScreen
    setCursor 1, 0, cursorPage
    PRINT 'Enter Your Name For Scoreboard:'
    setCursor 2, 0, cursorPage
    PRINT 'Press Enter To Accept...'
    ;mov cx, 607h
    ;mov ah, 1
    ;int 10h     ;set text-mode cursor shape (now - blinking)
    setCursor 20, 0, cursorPage
    PRINT 'You Have'
    setCursor 20, 12, cursorPage
    PRINT 'Characters Left.'
    setCursor 15, 15, cursorPage
    mov dx, 15  ;cursor position
    mov si, dx
GiveName:
    ;mov ah, 3
    ;mov bh, cursorPage
    ;int 10h     ;get cursor position (dl = row, dh = column)
    mov al, 25
    sub al, dl
    setCursor 20, 9, cursorPage
    cmp al, 10
    jz LeftWith10
    add al, 30h
    mov bh, cursorPage
    mov bl, 7   ;gray text
    mov cx, 1   ;write char 1 times
    mov ah, 9
    int 10h     ;write char
    setCursor 20, 10, cursorPage
    PRINT ' '
    jmp CharLeftFinishing
LeftWith10:
    PRINT '10'
CharLeftFinishing:
    mov dx, si
    setCursor 12, dl, cursorPage
    mov al, '_'
    mov bh, cursorPage
    mov bl, 7   ;gray text
    mov cx, 1   ;write char 1 times
    mov ah, 9
    int 10h     ;write cursor
    mov ah, 7
    int 21h     ;wait for keypress
    cmp al, 0Dh ;Enter
    jz AcceptName
    cmp al, 09h ;Tab
    jz GiveNameNext
    ;mov ah, 3
    ;mov bh, cursorPage
    ;int 10h     ;get cursor position (dl = row, dh = column)
    cmp dl, 15
    jnz CheckTooRight   ;do not go too left
    cmp al, 8   ;Backspace
    jz GiveNameNext
CheckTooRight:
    cmp dl, 25
    jnz WriteChar   ;do not go too right
    cmp al, 8   ;Backspace
    jnz GiveNameNext
WriteChar:
    mov bh, cursorPage
    mov bl, 7   ;gray text
    mov cx, 1   ;write char 1 times
    mov ah, 9
    int 10h     ;write char
    cmp al, 8   ;Backspace
    jnz NormalChar
    dec dl  ;position -1
    mov si, dx
    mov ax, 0A00h   ;ah = 0Ah, al = null
    mov bh, cursorPage
    mov cx, 1   ;write char 1 times
    int 10h     ;overwrite char
    setCursor 12, dl, cursorPage    ;move back
    ;mov ax, 0A00h   ;ah = 0Ah, al = null
    ;mov bh, cursorPage
    ;mov cx, 1   ;write char 1 times
    int 10h     ;overwrite char
    jmp GiveNameNext
NormalChar:
    inc dl  ;position +1
    mov si, dx
GiveNameNext:
    jmp GiveName
AcceptName:
    mov ax, 0A00h   ;ah = 0Ah, al = null
    mov bh, cursorPage
    mov cx, 1   ;write char 1 times
    int 10h     ;overwrite cursor ('_')

    lea si, GameData    ;Data is stored here
    mov cl, 15
ReadName:
    setCursor 12, cl, cursorPage    ;name start...
    mov ah, 8
    int 10h     ;read char
    mov [si], al    ;store char
    inc si
    inc cl
    cmp cl, 25
    jnz ReadName
    xor bx, bx
ReadScores:
    mov al, ScoresString[bx]
    mov [si], al
    inc si
    inc bx
    cmp bx, 5
    jnz ReadScores
ReadTime:
    mov al, Hours[0]
    mov [si], al
    mov al, Hours[1]
    mov [si+1], al
    mov [si+2], ':'
    mov al, Minutes[0]
    mov [si+3], al
    mov al, Minutes[1]
    mov [si+4], al
    mov [si+5], ':'
    mov al, Seconds[0]
    mov [si+6], al
    mov al, Seconds[1]
    mov [si+7], al
    ;GameData complete!
    mov offsetLOW, 713
    mov offsetHIGH, 0
    call FilePosition   ;713 bytes from file start
    mov cx, 23  ;23 bytes to write
    lea dx, GameData
    call WriteToFile    ;CX = number of bytes to write, DX = point to text

UpdateScoreBoard:
    mov offsetLOW, 0
    mov offsetHIGH, 0
    call FilePosition   ;file start
    mov cx, 736     ;736 bytes to read
    lea dx, ScoreData   ;scores 10-14 (2: 33-37) (+23)
    call ReadFromFile   ;CX = number of bytes to read, DX points to place to read to

    mov cx, 31  ;counters counter
StartScoreCheck:
    mov si, 10  ;main index
    xor di, di  ;GameData index
    mov bh, cl  ;GameData to check, main counter
    ;xor cx, cx  ;Check again flag - if 0 then continue
PreScoreCheck:
    mov bl, 5   ;secondery counter
ScoreCheck:
    mov al, ScoreData[si]
    cmp al, ScoreData[si+23]
    js FlipScorePositions
    ja AfterFlipping    ;Avoid double flipping!
    inc si
    dec bl
    jnz ScoreCheck
    jmp AfterFlipping

FlipScorePositions:
    ;inc cx
    push bx
    push cx
    push di
    xor bx, bx
    mov cx, 23
    ;0-22, 23-45 (+23)  GameData SOME BUGS HERE AT SORTING THE SCORES!
FlipProcess:
    mov al, ScoreData[di]
    mov GameData[bx], al    ;save last data
    mov al, ScoreData[di+23]
    mov ScoreData[di], al   ;copy bigger to it's place
    mov al, GameData[bx]
    mov ScoreData[di+23], al    ;retrive last data
    inc di
    inc bx
    loop FlipProcess
    pop di
    pop cx
    pop bx
AfterFlipping:
    add di, 23  ;next game data
    mov si, di
    add si, 10  ;points on next scores to check
    dec bh
    jnz PreScoreCheck
    ;cmp cx, 0
    ;jnz StartScoreCheck
    loop StartScoreCheck

    mov offsetLOW, 0
    mov offsetHIGH, 0
    call FilePosition   ;file start
    mov cx, 736 ;736 bytes to write
    lea dx, ScoreData
    call WriteToFile    ;CX = number of bytes to write, DX = point to text

EndYouLose:
ret
;********************************************************************

PrintScores:
    push ax
    push bx
    push cx
    push dx
    push bp
    push es

    setCursor 0, 0, cursorPage  ;scores position
    PRINT 'Scores: '
    mov ax, Scores
    xor dx, dx
    mov cx, 10          ;for decimal
    mov bx, 4           ;Index
DividingForDecimal:
    div cx
    add dl, 30h         ;to character (ascii number)
    mov ScoresString[bx], dl
    xor dx, dx
    dec bx
    cmp ax, 0
    jnz DividingForDecimal
    mov bh, cursorPage  ;page number
    mov ah, 3
    int 10h             ;get cursor position for dh and dl to match the position
    mov ax, 1301h       ;ah = 13h, al = 1
    mov cx, 5           ;number of characters to write
    mov bl, 7           ;attribute
    push ds
    pop es
    lea bp, ScoresString
    int 10h             ;write character with attribute

    pop es
    pop bp
    pop dx
    pop cx
    pop bx
    pop ax
ret
;********************************************************************

Start321:
    setCursor 12, 20, cursorPage
    PRINT '3'
    ;call BigDelay
    call SecondDelay
    setCursor 12, 20, cursorPage
    PRINT '2'
    ;call BigDelay
    call SecondDelay
    setCursor 12, 20, cursorPage
    PRINT '1'
    ;call BigDelay
    call SecondDelay
    call clearText
ret
;********************************************************************

PrintLives:
    push ax
    push bx
    push cx
    push dx
    push bp
    push es

    setCursor 24, 0, cursorPage  ;scores position
    PRINT 'Lives: '
    mov al, Lives
    xor ah, ah
    mov cl, 10          ;for decimal
    mov bx, 2           ;Index
LivesDividingForDecimal:
    div cl
    add ah, 30h         ;to character (ascii number)
    mov LivesString[bx], ah
    xor ah, ah
    dec bx
    cmp ax, 0
    jnz LivesDividingForDecimal
    mov bh, cursorPage  ;page number
    mov ah, 3
    int 10h             ;get cursor position for dh and dl to match the position
    mov ax, 1301h       ;ah = 13h, al = 1
    mov cx, 3           ;number of characters to write
    mov bl, 7           ;attribute
    push ds
    pop es
    lea bp, LivesString
    int 10h             ;write character with attribute

    pop es
    pop bp
    pop dx
    pop cx
    pop bx
    pop ax
ret
;********************************************************************

printLevelNum:
    push ax
    push bx
    push cx
    push dx

    mov al, 30h
    mov cx, 3           ;number of times to write character
    mov bl, 7           ;attribute
    mov bh, cursorPage  ;page number
    mov ah, 9
    int 10h             ;write character with attribute

    mov dl, 9
    mov al, Level
printNext:
    setCursor 1, dl, cursorPage
    xor ah, ah
    mov bl, 10
    div bl
    push ax
    mov al, ah
    add al, 30h         ;character
    mov cx, 1           ;number of times to write character
    mov bl, 7           ;attribute
    mov bh, cursorPage  ;page number
    mov ah, 9
    int 10h             ;write character with attribute
    dec dl              ;next character
    pop ax
    cmp al, 0
    jnz printNext

    pop dx
    pop cx
    pop bx
    pop ax
ret
;********************************************************************

WatchScoreBoard:
;0___4__________17______________32_______|
;               SCORE-BOARD:             |
;          Press any key to quit         |
;    name         score          time    |
;1.  ----------   -----          --:--:--|
;2.  ----------   -----          --:--:--|
;3.  ----------   -----          --:--:--|
;4.  ----------   -----          --:--:--|
;5.  ----------   -----          --:--:--|
;6.  ----------   -----          --:--:--|
;7.  ----------   -----          --:--:--|
;8.  ----------   -----          --:--:--|
;9.  ----------   -----          --:--:--|
;10. ----------   -----          --:--:--|
;________________________________________|
    push ax
    push bx
    push cx

    call clearEntireScreen
    setCursor 0, 15, cursorPage
    PRINT 'SCORE-BOARD'
    setCursor 1, 10, cursorPage
    PRINT 'Press any key to quit'
    mov ch, 4   ;top line of numbering
    mov cl, 1
    mov ah, 09h
    mov bl, 7   ;gray color
    mov bh, cursorPage
ScoreBoardNumbers:
    setCursor ch, 0, cursorPage ;names - 4, scores - 17, time - 32
    mov al, cl
    add al, 30h ;turn to ascii
    push cx
    mov cx, 1   ;write charcter 1 times
    int 10h     ;print the number
    pop cx
    setCursor ch, 1, cursorPage
    mov al, '.'
    push cx
    mov cx, 1   ;write charcter 1 times
    int 10h     ;print '.'
    pop cx
    add ch, 2
    inc cl
    cmp cl, 10  ;number of times to repeat
    jnz ScoreBoardNumbers
    setCursor ch, 0, cursorPage
    PRINT '10.' ;the last number on the scoreboard

    mov offsetLOW, 0
    mov offsetHIGH, 0
    call FilePosition   ;file start
    mov cx, 736     ;736 bytes to read
    lea dx, ScoreData   ;scores 10-14 (2: 33-37) (+23)
    call ReadFromFile   ;CX = number of bytes to read, DX points to place to read to

ScoreBoardNames:
    setCursor 2, 4, cursorPage  ;names - 4, scores - 17, times - 32
    PRINT 'Name'
    mov ch, 4
    xor si, si  ;name data
    mov ah, 09h
    mov bl, 7   ;gray color
    mov bh, cursorPage
ScoreBoardNamesLoop:
    mov cl, 4   ;cursor position
    setCursor ch, cl, cursorPage    ;names - 4, scores - 17, time - 32
ScoreBoardNamesInnerLoop:
    mov al, ScoreData[si]
    push cx
    mov cx, 1   ;write char 1 times
    int 10h ;write char
    pop cx
    inc cl  ;cursor position +1
    setCursor ch, cl, cursorPage
    inc si
    cmp cl, 14
    jnz ScoreBoardNamesInnerLoop
    add si, 13  ;next name
    add ch, 2
    cmp ch, 24  ;end of names
    jnz ScoreBoardNamesLoop

ScoreBoardScoers:
    setCursor 2, 17, cursorPage ;names - 4, scores - 17, times - 32
    PRINT 'Score'
    mov ch, 4
    mov si, 10  ;score data
    mov ah, 09h
    mov bl, 7   ;gray color
    mov bh, cursorPage
ScoreBoardScoersLoop:
    mov cl, 17  ;cursor position
    setCursor ch, cl, cursorPage    ;names - 4, scores - 17, time - 32
ScoreBoardScoersInnerLoop:
    mov al, ScoreData[si]
    push cx
    mov cx, 1   ;write char 1 times
    int 10h ;write char
    pop cx
    inc cl  ;cursor position +1
    setCursor ch, cl, cursorPage
    inc si
    cmp cl, 22
    jnz ScoreBoardScoersInnerLoop
    add si, 18  ;next score
    add ch, 2
    cmp ch, 24  ;end of scores
    jnz ScoreBoardScoersLoop

ScoreBoardTimes:
    setCursor 2, 32, cursorPage ;names - 4, scores - 17, times - 32
    PRINT 'Time'
    mov ch, 4
    mov si, 15  ;time data
    mov ah, 09h
    mov bl, 7   ;gray color
    mov bh, cursorPage
ScoreBoardTimesLoop:
    mov cl, 32  ;cursor position
    setCursor ch, cl, cursorPage    ;names - 4, scores - 17, time - 32
ScoreBoardTimesInnerLoop:
    mov al, ScoreData[si]
    push cx
    mov cx, 1   ;write char 1 times
    int 10h ;write char
    pop cx
    inc cl  ;cursor position +1
    setCursor ch, cl, cursorPage
    inc si
    cmp cl, 40
    jnz ScoreBoardTimesInnerLoop
    add si, 15  ;next time
    add ch, 2
    cmp ch, 24  ;end of times
    jnz ScoreBoardTimesLoop

    mov ah, 7
    int 21h
    call clearEntireScreen

    pop cx
    pop bx
    pop ax
    jmp PreMainMenu     ;start the game over...
ret
;********************************************************************

openSpeaker:
    push ax

    ; open speaker
    in al, 61h
    or al, 00000011b
    out 61h, al

    ; send control word to change frequency
    mov al, 0B6h
    out 43h, al

    mov al, 1
    mov SpeakerOn, al

    pop ax
ret
;********************************************************************

playSound:
    push ax
    ; play frequency
    mov ax, note
    out 42h, al ; Sending lower byte
    mov al, ah
    out 42h, al ; Sending upper byte
    pop ax
ret
;********************************************************************

closeSpeaker:
    push ax
    ; close the speaker
    in al, 61h
    and al, 11111100b
    out 61h, al

    xor al, al
    mov SpeakerOn, al
    pop ax
ret
;********************************************************************

TimeDisplay:
    push ax
    push bx
    push cx
    push dx
    push bp
    push es

    mov ah, 2Ch
    int 21h
    ;mov Seconds, dh
    ;mov Minutes, cl
    cmp FirstTime, 0
    jz DisplayingTime
    mov StartSeconds, dh
;    mov StartMinutes, cl
;    mov StartHour, ch
DisplayingTime:
;    mov dl, cl
;    sub ch, StartHour
;    jnc MinutesNext
;    ;neg ch      ;absolute Value
;    add ch, 60
;MinutesNext:
;    sub dl, StartMinutes
;    jnc SecondsNext
;    ;neg dl      ;absolute Value
;    add dl, 60
;SecondsNext:
    sub dh, StartSeconds
    jnc ProcNext
    ;neg dh      ;absolute Value
    add dh, 60
ProcNext:
    cmp dh, 1   ;For a minute to pass the seconds must be at least 1 before 0.
    jnz MinutesNeedNoPass
    mov MinutesCanPass, 1
MinutesNeedNoPass:
    mov dl, MinutesHEX
    cmp dh, 0   ;minutes passed
    jnz noNewMinute
    ;cmp FirstTime, 1
    ;jz noNewMinute
    cmp MinutesCanPass, 1
    jnz noNewMinute
    mov MinutesCanPass, 0
    inc dl
    mov MinutesHEX, dl
noNewMinute:
    cmp dl, 1   ;For a hour to pass the minutes must be at least 1 before 0.
    jnz HoursNeedNoPass
    mov HoursCanPass, 1
HoursNeedNoPass:
    mov ch, HoursHEX
    cmp dh, 0   ;minutes passed
    jnz noNewHour
    cmp dl, 60  ;hour passed
    jnz noNewHour
    ;cmp FirstTime, 1
    ;jz noNewHour
    cmp HoursCanPass, 1
    jnz noNewHour
    mov HoursCanPass, 0
    inc ch
    mov HoursHEX, ch
    sub dl, 60
    mov MinutesHEX, dl
noNewHour:
    mov bl, 10
    xor ah, ah
    mov al, ch  ;hours
    div bl      ;ah = first number, al = second number
    add al, 30h ;to ascii
    add ah, 30h ;to ascii
    mov Hours[1], ah
    mov Hours[0], al
    xor ah, ah
    mov al, dl  ;minutes
    div bl      ;ah = first number, al = second number
    add al, 30h ;to ascii
    add ah, 30h ;to ascii
    mov Minutes[1], ah
    mov Minutes[0], al
    xor ah, ah
    mov al, dh  ;seconds
    div bl      ;ah = first number, al = second number
    add al, 30h ;to ascii
    add ah, 30h ;to ascii
    mov Seconds[1], ah
    mov Seconds[0], al
    setCursor 0, 26, cursorPage
    PRINT 'Time: '
    push ds
    pop es          ;es = ds
    mov ax, 1301h   ;ah = 13h, al = 1
    mov bh, cursorPage
    mov bl, 7       ;gray
    mov cx, 2       ;only 2 charcter to write
    xor dh, dh      ;row 0
    mov dl, 32      ;column 32
    lea bp, Hours
    int 10h         ;write string
    PRINT ':'
    mov dl, 35      ;column 35
    lea bp, Minutes
    int 10h         ;write string
    PRINT ':'
    ;mov ax, 1301h   ;ah = 13h, al = 1
    ;mov bh, cursorPage
    ;mov bl, 7       ;gray
    ;mov cx, 2       ;only 2 charcter to write
    ;xor dh, dh      ;row 0
    mov dl, 38      ;column 38
    lea bp, Seconds
    int 10h         ;write string

    mov FirstTime, 0

    pop es
    pop bp
    pop dx
    pop cx
    pop bx
    pop ax
ret
;********************************************************************

;Open file (if exist)  or Create file (if not exist):
;DX must point on file name!
proc GetHandle
    push ax
    push cx

OpenFile:
    mov al, 2   ;read/write
    mov ah, 3Dh
    int 21h     ;Open file -> AX = file handle
    jc CreateFile
    mov Handle, ax

    pop cx
    pop ax
    ret

CreateFile:
    xor cx, cx  ;Normal file - 0
    mov ah, 3Ch
    int 21h     ;Create file -> AX = file handle
    jc FileError
    mov Handle, ax

    pop cx
    pop ax
    ret
endp GetHandle
;********************************************************************

;Write to file:
;number of bytes to write must be in CX and DX points on the text to write!
proc WriteToFile
    push ax
    push bx

    mov bx, Handle  ;Which file
    mov ah, 40h
    int 21h     ;Write to file from WriteData
    jc FileError

    pop bx
    pop ax
    ret
endp WriteToFile
;********************************************************************

;Read from file
;number of bytes to read must be in CX and DX points on the place to read to!
proc ReadFromFile
    push ax
    push bx

    mov bx, Handle  ;Which file
    mov ah, 3Fh
    int 21h     ;Read from file to ReadData
    jc FileError

    pop bx
    pop ax
    ret
endp ReadFromFile
;********************************************************************

;Close file:
proc CloseFile
    push ax
    push bx

    mov bx, Handle
    mov ah, 3Eh
    int 21h
    jc FileError

    pop bx
    pop ax
    ret
endp CloseFile
;********************************************************************

;Move "cursor" position in file
proc FilePosition
    push ax
    push bx
    push cx
    push dx

    mov bx, Handle  ;Which file
    mov dx, offsetLOW   ;offset from file start
    mov cx, offsetHIGH  ;offset from file start
    mov ax, 4200h   ;AL = 0 - start of file
    int 21h
    jc FileError

    pop dx
    pop cx
    pop bx
    pop ax
    ret
endp FilePosition
;********************************************************************

END START               ;compiler stops to compile here (End of program)
