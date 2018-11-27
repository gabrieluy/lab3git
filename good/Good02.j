.class public Good02
.super java/lang/Object

.method public static main([Ljava/lang/String;)V
.limit locals 2
.limit stack  1000
invokestatic Runtime/readInt()I
istore 1
label0:
bipush 1
iload 1
ldc 8
if_icmplt label2
pop
bipush 0
label2:
ifeq label1
iload 1
invokestatic Runtime/writeInt(I)V
iload 1
ldc 1
iadd
istore 1
goto label0
label1:
return
.end method

