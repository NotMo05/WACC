.intel_syntax noprefix
.globl main
.section .rodata
.text
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov edi, 42 
and rsp, -16 
call exit@plt
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov edi, -1 
and rsp, -16 
call exit@plt
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov edi, 7 
and rsp, -16 
call exit@plt
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov edi, 256 
and rsp, -16 
call exit@plt
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 8 
mov edi, 4 
call _malloc
mov dword ptr [rax], 0 
add eax, 4 
mov qword ptr [rbp - 8], rax 
mov rax, 0 
add rsp, 8 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 9 
mov edi, 4 
call _malloc
mov dword ptr [rax], 0 
add eax, 4 
mov qword ptr [rbp - 8], rax 
mov byte ptr [rbp - 9], 1 
mov rax, 0 
add rsp, 9 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 8 
mov edi, 12 
call _malloc
mov dword ptr [rax], 1 
add eax, 4 
mov qword ptr [rax], 0 
mov qword ptr [rbp - 8], rax 
mov rax, 0 
add rsp, 8 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 8 
mov edi, 4 
call _malloc
mov dword ptr [rax], 0 
add eax, 4 
mov qword ptr [rbp - 8], rax 
mov rax, 0 
add rsp, 8 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 1 
mov r10d, 128 
mov byte ptr [rbp - 1], r10d 
mov rax, 0 
add rsp, 1 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 1 
mov r10d, -1 
mov byte ptr [rbp - 1], r10d 
mov rax, 0 
add rsp, 1 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 12 
mov qword ptr [rbp - 8], 0 
mov r10, qword ptr [rbp - 8] 
cmp r10, 0 
je_errNull
mov r10, qword ptr [r10 + 8] 
mov dword ptr [rbp - 12], r10d 
mov rax, 0 
add rsp, 12 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 12 
mov qword ptr [rbp - 8], 0 
mov r10, qword ptr [rbp - 8] 
cmp r10, 0 
je_errNull
mov r10, qword ptr [r10] 
mov dword ptr [rbp - 12], r10d 
mov rax, 0 
add rsp, 12 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 4 
mov dword ptr [rbp - 4], 5 
mov r10d, dword ptr [rbp - 4] 
mov edi, r10d 
and rsp, -16 
call exit@plt
mov rax, 0 
add rsp, 4 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 1028 
mov dword ptr [rbp - 4], 0 
mov dword ptr [rbp - 8], 1 
mov dword ptr [rbp - 12], 2 
mov dword ptr [rbp - 16], 3 
mov dword ptr [rbp - 20], 4 
mov dword ptr [rbp - 24], 5 
mov dword ptr [rbp - 28], 6 
mov dword ptr [rbp - 32], 7 
mov dword ptr [rbp - 36], 8 
mov dword ptr [rbp - 40], 9 
mov dword ptr [rbp - 44], 10 
mov dword ptr [rbp - 48], 11 
mov dword ptr [rbp - 52], 12 
mov dword ptr [rbp - 56], 13 
mov dword ptr [rbp - 60], 14 
mov dword ptr [rbp - 64], 15 
mov dword ptr [rbp - 68], 16 
mov dword ptr [rbp - 72], 17 
mov dword ptr [rbp - 76], 18 
mov dword ptr [rbp - 80], 19 
mov dword ptr [rbp - 84], 20 
mov dword ptr [rbp - 88], 21 
mov dword ptr [rbp - 92], 22 
mov dword ptr [rbp - 96], 23 
mov dword ptr [rbp - 100], 24 
mov dword ptr [rbp - 104], 25 
mov dword ptr [rbp - 108], 26 
mov dword ptr [rbp - 112], 27 
mov dword ptr [rbp - 116], 28 
mov dword ptr [rbp - 120], 29 
mov dword ptr [rbp - 124], 30 
mov dword ptr [rbp - 128], 31 
mov dword ptr [rbp - 132], 32 
mov dword ptr [rbp - 136], 33 
mov dword ptr [rbp - 140], 34 
mov dword ptr [rbp - 144], 35 
mov dword ptr [rbp - 148], 36 
mov dword ptr [rbp - 152], 37 
mov dword ptr [rbp - 156], 38 
mov dword ptr [rbp - 160], 39 
mov dword ptr [rbp - 164], 40 
mov dword ptr [rbp - 168], 41 
mov dword ptr [rbp - 172], 42 
mov dword ptr [rbp - 176], 43 
mov dword ptr [rbp - 180], 44 
mov dword ptr [rbp - 184], 45 
mov dword ptr [rbp - 188], 46 
mov dword ptr [rbp - 192], 47 
mov dword ptr [rbp - 196], 48 
mov dword ptr [rbp - 200], 49 
mov dword ptr [rbp - 204], 50 
mov dword ptr [rbp - 208], 51 
mov dword ptr [rbp - 212], 52 
mov dword ptr [rbp - 216], 53 
mov dword ptr [rbp - 220], 54 
mov dword ptr [rbp - 224], 55 
mov dword ptr [rbp - 228], 56 
mov dword ptr [rbp - 232], 57 
mov dword ptr [rbp - 236], 58 
mov dword ptr [rbp - 240], 59 
mov dword ptr [rbp - 244], 60 
mov dword ptr [rbp - 248], 61 
mov dword ptr [rbp - 252], 62 
mov dword ptr [rbp - 256], 63 
mov dword ptr [rbp - 260], 64 
mov dword ptr [rbp - 264], 65 
mov dword ptr [rbp - 268], 66 
mov dword ptr [rbp - 272], 67 
mov dword ptr [rbp - 276], 68 
mov dword ptr [rbp - 280], 69 
mov dword ptr [rbp - 284], 70 
mov dword ptr [rbp - 288], 71 
mov dword ptr [rbp - 292], 72 
mov dword ptr [rbp - 296], 73 
mov dword ptr [rbp - 300], 74 
mov dword ptr [rbp - 304], 75 
mov dword ptr [rbp - 308], 76 
mov dword ptr [rbp - 312], 77 
mov dword ptr [rbp - 316], 78 
mov dword ptr [rbp - 320], 79 
mov dword ptr [rbp - 324], 80 
mov dword ptr [rbp - 328], 81 
mov dword ptr [rbp - 332], 82 
mov dword ptr [rbp - 336], 83 
mov dword ptr [rbp - 340], 84 
mov dword ptr [rbp - 344], 85 
mov dword ptr [rbp - 348], 86 
mov dword ptr [rbp - 352], 87 
mov dword ptr [rbp - 356], 88 
mov dword ptr [rbp - 360], 89 
mov dword ptr [rbp - 364], 90 
mov dword ptr [rbp - 368], 91 
mov dword ptr [rbp - 372], 92 
mov dword ptr [rbp - 376], 93 
mov dword ptr [rbp - 380], 94 
mov dword ptr [rbp - 384], 95 
mov dword ptr [rbp - 388], 96 
mov dword ptr [rbp - 392], 97 
mov dword ptr [rbp - 396], 98 
mov dword ptr [rbp - 400], 99 
mov dword ptr [rbp - 404], 100 
mov dword ptr [rbp - 408], 101 
mov dword ptr [rbp - 412], 102 
mov dword ptr [rbp - 416], 103 
mov dword ptr [rbp - 420], 104 
mov dword ptr [rbp - 424], 105 
mov dword ptr [rbp - 428], 106 
mov dword ptr [rbp - 432], 107 
mov dword ptr [rbp - 436], 108 
mov dword ptr [rbp - 440], 109 
mov dword ptr [rbp - 444], 110 
mov dword ptr [rbp - 448], 111 
mov dword ptr [rbp - 452], 112 
mov dword ptr [rbp - 456], 113 
mov dword ptr [rbp - 460], 114 
mov dword ptr [rbp - 464], 115 
mov dword ptr [rbp - 468], 116 
mov dword ptr [rbp - 472], 117 
mov dword ptr [rbp - 476], 118 
mov dword ptr [rbp - 480], 119 
mov dword ptr [rbp - 484], 120 
mov dword ptr [rbp - 488], 121 
mov dword ptr [rbp - 492], 122 
mov dword ptr [rbp - 496], 123 
mov dword ptr [rbp - 500], 124 
mov dword ptr [rbp - 504], 125 
mov dword ptr [rbp - 508], 126 
mov dword ptr [rbp - 512], 127 
mov dword ptr [rbp - 516], 128 
mov dword ptr [rbp - 520], 129 
mov dword ptr [rbp - 524], 130 
mov dword ptr [rbp - 528], 131 
mov dword ptr [rbp - 532], 132 
mov dword ptr [rbp - 536], 133 
mov dword ptr [rbp - 540], 134 
mov dword ptr [rbp - 544], 135 
mov dword ptr [rbp - 548], 136 
mov dword ptr [rbp - 552], 137 
mov dword ptr [rbp - 556], 138 
mov dword ptr [rbp - 560], 139 
mov dword ptr [rbp - 564], 140 
mov dword ptr [rbp - 568], 141 
mov dword ptr [rbp - 572], 142 
mov dword ptr [rbp - 576], 143 
mov dword ptr [rbp - 580], 144 
mov dword ptr [rbp - 584], 145 
mov dword ptr [rbp - 588], 146 
mov dword ptr [rbp - 592], 147 
mov dword ptr [rbp - 596], 148 
mov dword ptr [rbp - 600], 149 
mov dword ptr [rbp - 604], 150 
mov dword ptr [rbp - 608], 151 
mov dword ptr [rbp - 612], 152 
mov dword ptr [rbp - 616], 153 
mov dword ptr [rbp - 620], 154 
mov dword ptr [rbp - 624], 155 
mov dword ptr [rbp - 628], 156 
mov dword ptr [rbp - 632], 157 
mov dword ptr [rbp - 636], 158 
mov dword ptr [rbp - 640], 159 
mov dword ptr [rbp - 644], 160 
mov dword ptr [rbp - 648], 161 
mov dword ptr [rbp - 652], 162 
mov dword ptr [rbp - 656], 163 
mov dword ptr [rbp - 660], 164 
mov dword ptr [rbp - 664], 165 
mov dword ptr [rbp - 668], 166 
mov dword ptr [rbp - 672], 167 
mov dword ptr [rbp - 676], 168 
mov dword ptr [rbp - 680], 169 
mov dword ptr [rbp - 684], 170 
mov dword ptr [rbp - 688], 171 
mov dword ptr [rbp - 692], 172 
mov dword ptr [rbp - 696], 173 
mov dword ptr [rbp - 700], 174 
mov dword ptr [rbp - 704], 175 
mov dword ptr [rbp - 708], 176 
mov dword ptr [rbp - 712], 177 
mov dword ptr [rbp - 716], 178 
mov dword ptr [rbp - 720], 179 
mov dword ptr [rbp - 724], 180 
mov dword ptr [rbp - 728], 181 
mov dword ptr [rbp - 732], 182 
mov dword ptr [rbp - 736], 183 
mov dword ptr [rbp - 740], 184 
mov dword ptr [rbp - 744], 185 
mov dword ptr [rbp - 748], 186 
mov dword ptr [rbp - 752], 187 
mov dword ptr [rbp - 756], 188 
mov dword ptr [rbp - 760], 189 
mov dword ptr [rbp - 764], 190 
mov dword ptr [rbp - 768], 191 
mov dword ptr [rbp - 772], 192 
mov dword ptr [rbp - 776], 193 
mov dword ptr [rbp - 780], 194 
mov dword ptr [rbp - 784], 195 
mov dword ptr [rbp - 788], 196 
mov dword ptr [rbp - 792], 197 
mov dword ptr [rbp - 796], 198 
mov dword ptr [rbp - 800], 199 
mov dword ptr [rbp - 804], 200 
mov dword ptr [rbp - 808], 201 
mov dword ptr [rbp - 812], 202 
mov dword ptr [rbp - 816], 203 
mov dword ptr [rbp - 820], 204 
mov dword ptr [rbp - 824], 205 
mov dword ptr [rbp - 828], 206 
mov dword ptr [rbp - 832], 207 
mov dword ptr [rbp - 836], 208 
mov dword ptr [rbp - 840], 209 
mov dword ptr [rbp - 844], 210 
mov dword ptr [rbp - 848], 211 
mov dword ptr [rbp - 852], 212 
mov dword ptr [rbp - 856], 213 
mov dword ptr [rbp - 860], 214 
mov dword ptr [rbp - 864], 215 
mov dword ptr [rbp - 868], 216 
mov dword ptr [rbp - 872], 217 
mov dword ptr [rbp - 876], 218 
mov dword ptr [rbp - 880], 219 
mov dword ptr [rbp - 884], 220 
mov dword ptr [rbp - 888], 221 
mov dword ptr [rbp - 892], 222 
mov dword ptr [rbp - 896], 223 
mov dword ptr [rbp - 900], 224 
mov dword ptr [rbp - 904], 225 
mov dword ptr [rbp - 908], 226 
mov dword ptr [rbp - 912], 227 
mov dword ptr [rbp - 916], 228 
mov dword ptr [rbp - 920], 229 
mov dword ptr [rbp - 924], 230 
mov dword ptr [rbp - 928], 231 
mov dword ptr [rbp - 932], 232 
mov dword ptr [rbp - 936], 233 
mov dword ptr [rbp - 940], 234 
mov dword ptr [rbp - 944], 235 
mov dword ptr [rbp - 948], 236 
mov dword ptr [rbp - 952], 237 
mov dword ptr [rbp - 956], 238 
mov dword ptr [rbp - 960], 239 
mov dword ptr [rbp - 964], 240 
mov dword ptr [rbp - 968], 241 
mov dword ptr [rbp - 972], 242 
mov dword ptr [rbp - 976], 243 
mov dword ptr [rbp - 980], 244 
mov dword ptr [rbp - 984], 245 
mov dword ptr [rbp - 988], 246 
mov dword ptr [rbp - 992], 247 
mov dword ptr [rbp - 996], 248 
mov dword ptr [rbp - 1000], 249 
mov dword ptr [rbp - 1004], 250 
mov dword ptr [rbp - 1008], 251 
mov dword ptr [rbp - 1012], 252 
mov dword ptr [rbp - 1016], 253 
mov dword ptr [rbp - 1020], 254 
mov dword ptr [rbp - 1024], 255 
mov dword ptr [rbp - 1028], 256 
mov rax, 0 
add rsp, 1028 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 4 
mov dword ptr [rbp - 4], 42 
mov rax, 0 
add rsp, 4 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 4 
mov dword ptr [rbp - 4], -1 
mov rax, 0 
add rsp, 4 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 1 
mov byte ptr [rbp - 1], 97 
mov rax, 0 
add rsp, 1 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 4 
mov dword ptr [rbp - 4], 0 
mov rax, 0 
add rsp, 4 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 1 
mov byte ptr [rbp - 1], 77 
mov rax, 0 
add rsp, 1 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 1 
mov byte ptr [rbp - 1], 0 
mov rax, 0 
add rsp, 1 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 1 
mov byte ptr [rbp - 1], 33 
mov rax, 0 
add rsp, 1 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 1 
mov byte ptr [rbp - 1], 122 
mov rax, 0 
add rsp, 1 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 1 
mov byte ptr [rbp - 1], 1 
mov rax, 0 
add rsp, 1 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 4 
mov dword ptr [rbp - 4], 19 
mov r10d, dword ptr [rbp - 4] 
mov edi, r10d 
and rsp, -16 
call exit@plt
mov rax, 0 
add rsp, 4 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 16 
mov edi, 16 
call _malloc
mov qword ptr [rax], 10 
mov qword ptr [rax + 8], 97 
mov qword ptr [rbp - 8], rax 
mov r10, qword ptr [rbp - 8] 
mov qword ptr [rbp - 16], r10 
mov rax, 0 
add rsp, 16 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 8 
mov edi, 16 
call _malloc
mov qword ptr [rax], 10 
mov qword ptr [rax + 8], 97 
mov qword ptr [rbp - 8], rax 
mov rax, 0 
add rsp, 8 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 8 
mov edi, 16 
call _malloc
mov qword ptr [rax], 10 
mov qword ptr [rax + 8], 3 
mov qword ptr [rbp - 8], rax 
mov rax, 0 
add rsp, 8 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 8 
mov edi, 16 
call _malloc
mov qword ptr [rax], 97 
mov qword ptr [rax + 8], 98 
mov qword ptr [rbp - 8], rax 
mov rax, 0 
add rsp, 8 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 16 
mov edi, 16 
call _malloc
mov qword ptr [rax], 2 
mov qword ptr [rax + 8], 3 
mov qword ptr [rbp - 8], rax 
mov edi, 16 
call _malloc
mov qword ptr [rax], 1 
mov r10, qword ptr [rbp - 8] 
mov qword ptr [rax + 8], r10 
mov qword ptr [rbp - 16], rax 
mov rax, 0 
add rsp, 16 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 1 
mov byte ptr [rbp - 1], 97 
mov byte ptr [rbp - 1], 90 
mov rax, 0 
add rsp, 1 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 4 
mov dword ptr [rbp - 4], 10 
mov dword ptr [rbp - 4], 20 
mov r10d, dword ptr [rbp - 4] 
mov edi, r10d 
and rsp, -16 
call exit@plt
mov rax, 0 
add rsp, 4 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 1 
mov byte ptr [rbp - 1], 0 
mov byte ptr [rbp - 1], 1 
mov rax, 0 
add rsp, 1 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 4 
mov r10d, 1 
sub r10d, 2 
add r10d, 3 
sub r10d, 4 
add r10d, 5 
sub r10d, 6 
add r10d, 7 
sub r10d, 8 
add r10d, 9 
sub r10d, 10 
add r10d, 11 
sub r10d, 12 
add r10d, 13 
sub r10d, 14 
add r10d, 15 
sub r10d, 16 
add r10d, 17 
mov dword ptr [rbp - 4], r10d 
mov r10d, dword ptr [rbp - 4] 
mov edi, r10d 
and rsp, -16 
call exit@plt
mov rax, 0 
add rsp, 4 
pop rbp
ret
