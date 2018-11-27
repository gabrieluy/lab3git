.class public Good13
.super java/lang/Object

.method public static main([Ljava/lang/String;)V
.limit locals 4
.limit stack  1000
ldc 6
istore 1
iload 1
ldc 7
iadd
i2d
dstore 2
dload 2
invokestatic Runtime/writeReal(D)V
iload 1
i2d
ldc2_w 7.3
dadd
dstore 2
dload 2
invokestatic Runtime/writeReal(D)V
bipush 1
dload 2
iload 1
i2d
dcmpg
iflt label2
pop
bipush 0
label2:
ifeq label0
ldc "mal"
invokestatic Runtime/writeStr(Ljava/lang/String;)V
goto label1
label0:
ldc "bien"
invokestatic Runtime/writeStr(Ljava/lang/String;)V
label1:
iload 1
i2d
invokestatic Runtime/writeReal(D)V
return
.end method

