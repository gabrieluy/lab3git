.class public Good03
.super java/lang/Object

.method public static main([Ljava/lang/String;)V
.limit locals 4
.limit stack  1000
invokestatic Runtime/readInt()I
istore 1
ldc 1
istore 2
ldc 1
istore 3
label0:
bipush 1
iload 3
iload 1
ldc 1
iadd
if_icmplt label2
pop
bipush 0
label2:
ifeq label1
iload 3
iload 2
imul
istore 2
iload 3
ldc 1
iadd
istore 3
goto label0
label1:
iload 2
invokestatic Runtime/writeInt(I)V
return
.end method

