Test Suite for Iphigeneia
=========================

This test suite is in the format of Falderal version 0.9.

    -> Functionality "Interpret Iphigeneia program" is implemented by
    -> shell command "bin/iphi %(test-body-file)"

    -> Tests for functionality "Interpret Iphigeneia program"

Test 'var ... in ...' and assignment.

    | var k in k := 5
    = k=5

Test 'begin ... end'.

    | var k in begin
    |     k := 5;
    |     k := k + 1
    | end
    = k=6

Test nested 'var ... in ...' and arithmetic operators.

    | var i in var j in var k in begin
    |     i := 2;
    |     j := 3;
    |     k := i + j;   (*  5 *)
    |     i := j * k;   (* 15 *)
    |     j := i / 2;   (*  7 *)
    |     j := j - 1    (*  6 *)
    | end
    = j=6
    = i=15
    = k=5

Test 'if ... then ... else' command with negative result.

    | var i in var j in begin
    |     i := 2;
    |     if i > 4 then
    |       j := i * 2
    |     else
    |       j := i + 1
    | end
    = j=3
    = i=2

Test 'if ... then ... else' command with positive result.

    | var i in var j in begin
    |     i := 2;
    |     j := 1;
    |     if i < 4 & j = 1 then
    |       j := i * 6
    |     else
    |       j := i + 1
    | end
    = j=12
    = i=2

Test 'while ... do ...'.

    | var i in var j in begin
    |     i := 100;
    |     j := 0;
    |     while i > 0 do begin
    |         j := j + i;
    |         i := i - 1
    |     end
    | end
    = i=0
    = j=5050

Test 'while ... do ...'.

    | var a in var b in var c in
    | begin
    |     a := 10;
    |     b := 1;
    |     c := 2;
    |     while a > 0 do
    |         begin
    |             b := b * c;
    |             c := c + b;
    |             a := a - 1
    |         end
    | end
    = a=0
    = c=140982598893793678070294688422804665931354981644880911847733136248186424030732278900819020480668973702640170212905160639132296847654374706155245147715674612235227680384069415566749494180212370357849936526549755341591854042821940420766722160615645816921368300
    = b=140982598893793678070294688422804665931354981644880911847733136248186424030732278900819020480668973702640170212905160639132296847278898210361175931159590631877400396153764977561991761037132722898953457959352992281368361865140291306311370294857131871923863552

Test 'if ... then ... else' expression with negative result.

    | var a in var b in var c in
    | begin
    |     a := 10;
    |     b := 2;
    |     c := if a > 20 then a - b else a / b
    | end
    = c=5
    = b=2
    = a=10

Test 'if ... then ... else' expression with positive result.

    | var a in var b in var c in
    | begin
    |     a := 10;
    |     b := 2;
    |     c := if a < 20 then a - b else a / b
    | end
    = c=8
    = b=2
    = a=10

Test 'let ... in ...'.

    | var a in a := let b = 7 in 10 - b;
    = a=3

Test 'valueof ... in ...'.

    | var a in var b in begin
    |     a := 10;
    |     b := valueof c in begin
    |         c := a * 2
    |     end + 7
    | end
    = b=27
    = a=10

Test that 'var ... in ...' does not shadow.

    | var a in var b in
    | begin
    |     a := 1;
    |     b := 2;
    |     var a in
    |         a := 3
    | end
    = ["Variable a already declared"]

Test that 'let ... in ...' does shadow.

    | var a in var b in
    | begin
    |     a := 2;
    |     b := 3;
    |     a := let b = 7 in a * b
    | end
    = a=14
    = b=3

Test 'loop ...' and 'repeat'.

    | var a in a :=
    |     let c = 5 in let d = 1 in
    |         loop
    |             if c = 0 then
    |                 d
    |             else
    |                 let d = d * c in
    |                     let c = c - 1 in
    |                         repeat
    = a=120
