.class public Good11
.super java/lang/Object

.method public static main([Ljava/lang/String;)V
.limit locals 3
.limit stack  1000
bipush 1
ldc 1
ifne label0
ldc 1
invokestatic Runtime/writeIntBool(I)I
ifne label0
pop
bipush 0
label0:
istore 1
bipush 1
ldc 0
ifne label1
ldc 2
invokestatic Runtime/writeIntBool(I)I
ifne label1
pop
bipush 0
label1:
istore 1
bipush 0
ldc 1
ifeq label2
ldc 3
invokestatic Runtime/writeIntBool(I)I
ifeq label2
pop
bipush 1
label2:
istore 1
bipush 0
ldc 0
ifeq label3
ldc 4
invokestatic Runtime/writeIntBool(I)I
ifeq label3
pop
bipush 1
label3:
istore 1
return
.end method

