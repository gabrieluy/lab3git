.class public Good05
.super java/lang/Object

.method public static main([Ljava/lang/String;)V
.limit locals 4
.limit stack  1000
ldc 1
istore 1
iload 1
istore 2
invokestatic Runtime/readInt()I
istore 3
iload 1
invokestatic Runtime/writeInt(I)V
label0:
bipush 1
iload 2
iload 3
if_icmplt label2
pop
bipush 0
label2:
ifeq label1
iload 2
invokestatic Runtime/writeInt(I)V
iload 1
iload 2
iadd
istore 2
iload 2
iload 1
isub
istore 1
goto label0
label1:
return
.end method

