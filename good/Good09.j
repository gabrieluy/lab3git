.class public Good09
.super java/lang/Object

.method public static main([Ljava/lang/String;)V
.limit locals 5
.limit stack  1000
invokestatic Runtime/readInt()I
istore 1
ldc 2
istore 2
label0:
bipush 1
iload 2
iload 1
if_icmple label2
pop
bipush 0
label2:
ifeq label1
ldc 1
istore 4
ldc 2
istore 3
label3:
bipush 0
bipush 1
iload 3
iload 3
imul
iload 2
if_icmple label6
pop
bipush 0
label6:
ifeq label5
iload 4
ifeq label5
pop
bipush 1
label5:
ifeq label4
bipush 1
iload 2
iload 3
idiv
iload 3
imul
iload 2
if_icmpeq label9
pop
bipush 0
label9:
ifeq label7
ldc 0
istore 4
goto label8
label7:
iload 3
ldc 1
iadd
istore 3
label8:
goto label3
label4:
bipush 0
iload 4
ifeq label12
bipush 1
iload 1
iload 2
idiv
iload 2
imul
iload 1
if_icmpeq label13
pop
bipush 0
label13:
ifeq label12
pop
bipush 1
label12:
ifeq label10
iload 2
invokestatic Runtime/writeInt(I)V
iload 1
iload 2
idiv
istore 1
goto label11
label10:
iload 2
ldc 1
iadd
istore 2
label11:
goto label0
label1:
return
.end method

