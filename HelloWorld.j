.class public HelloWord
.super java/lang/Object

.method public static main([Ljava/lang/String;)V
.limit locals 2
.limit stack  1000
ldc 23
istore 1
iload 1
invokestatic Runtime/writeInt(I)V
bipush 1
ldc2_w 2.24
ldc2_w 2.24
dcmpg
ifeq label2
pop
bipush 0
label2:
ifeq label0
ldc 23
istore 1
goto label1
label0:
ldc 24
istore 1
label1:
return
.end method

