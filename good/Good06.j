.class public Good06
.super java/lang/Object

.method public static main([Ljava/lang/String;)V
.limit locals 3
.limit stack  1000
invokestatic Runtime/readInt()I
istore 1
iload 1
ldc 2
idiv
istore 2
label0:
bipush 1
iload 2
ldc 1
if_icmpgt label2
pop
bipush 0
label2:
ifeq label1
bipush 1
iload 2
iload 1
iload 2
idiv
imul
iload 1
if_icmpeq label5
pop
bipush 0
label5:
ifeq label3
iload 2
invokestatic Runtime/writeInt(I)V
goto label4
label3:
label4:
iload 2
ldc 1
isub
istore 2
goto label0
label1:
return
.end method

