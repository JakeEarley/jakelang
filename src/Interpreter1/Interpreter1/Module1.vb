Imports System.IO
Imports System.Windows.Forms

Module Module1

    Dim program As StreamReader
    Dim proglen As Integer = 0
    Dim prog(102800) As String
    Dim pathname As String
    Dim tempstorage As String
    Dim intstore As New Dictionary(Of String, Integer)
    Dim stringstore As New Dictionary(Of String, String)
    Dim inintcnt As Integer = 1
    Dim repeat As Boolean = False
    Dim _if As Boolean = True
    Dim checkcustomlines As Boolean = False
    Dim repetitions As Integer
    Dim outcome As Boolean = False
    Dim cladd As Integer = 0
    Dim checkelse As Boolean = False
    Dim invertoutcome As Boolean = True
    Dim showcommand As Boolean = False
    Dim _for As Boolean = False
    Dim intstep As Integer
    Dim firstint As Integer
    Dim secondint As Integer
    Dim forvarname As String
    Dim setalert As String = ""

    Sub Main()

        getProgram()
        findProgLen()
        parseToArray()
        parseArray()

        Console.ReadLine()
    End Sub

    Sub getProgram()
        Console.Write("Compile: ")
        pathname = Console.ReadLine()
        If pathname.Length >= 10 Then
            If pathname.Substring(pathname.Length - 9, 9) = ".jakelang" Then
                Try
                    program = New StreamReader(pathname)
                Catch fileNotFound As FileNotFoundException
                    Console.WriteLine("File not found. Creating new file of that name... ")
                    Dim filecreator As FileStream = File.Create(pathname)
                    Console.WriteLine("File created! Loading " & pathname)
                    filecreator.Close()
                Finally
                    program = New StreamReader(pathname)
                    Console.WriteLine("Successfully loaded " & pathname)
                End Try
            Else
                Try
                    program = New StreamReader(pathname & ".jakelang")
                Catch fileNotFound As FileNotFoundException
                    Console.WriteLine("File not found. Creating new file of that name... ")
                    Dim filecreator As FileStream = File.Create(pathname & ".jakelang")
                    Console.WriteLine("File created! Loading " & pathname & ".jakelang")
                    filecreator.Close()
                Finally
                    program = New StreamReader(pathname & ".jakelang")
                    Console.WriteLine("Successfully loaded " & pathname & ".jakelang")
                End Try
            End If
        Else
            Try
                program = New StreamReader(pathname & ".jakelang")
            Catch fileNotFound As FileNotFoundException
                Console.WriteLine("File not found. Creating new file of that name... ")
                Dim filecreator As FileStream = File.Create(pathname & ".jakelang")
                Console.WriteLine("File created! Loading " & pathname & ".jakelang")
                filecreator.Close()
            Finally
                program = New StreamReader(pathname & ".jakelang")
                Console.WriteLine("Successfully loaded " & pathname & ".jakelang")
            End Try
        End If

    End Sub

    Sub findProgLen()
        While Not program.EndOfStream
            program.ReadLine()
            proglen += 1
        End While
        program.BaseStream.Position = 0
    End Sub

    Sub parseToArray()
        program.BaseStream.Position = 0
        For spotlight As Integer = 1 To proglen Step 1
            prog(spotlight) = program.ReadLine()
        Next
        program.BaseStream.Position = 0
    End Sub

    Sub parseArray()
        Dim command As String
        Dim oldcommand As String
        Dim cmdlen As Integer



        For i As Integer = 1 To proglen Step 1
            If Not checkcustomlines Then

                command = prog(i).ToLower()
                oldcommand = prog(i)
                cmdlen = command.Length()

                If command.Length < 2 Then

                ElseIf command.Substring(0, 1) = ">" Then

                Else
                    runCommands(command, oldcommand, i)
                End If
            End If
            Do While checkcustomlines
                cladd += 1
                command = prog(i + cladd).ToLower()
                oldcommand = prog(i + cladd)


                If command.Length < 1 Then
                ElseIf command.Substring(0, 1) = ">" Then
                    command = command.Substring(1, command.Length - 1)

                    oldcommand = oldcommand.Substring(1, command.Length - 1)

                    If repeat Then
                        For rep As Integer = 1 To repetitions

                            runCommands(command, oldcommand, i + cladd)
                        Next
                    End If

                    If _if Then
                        If outcome = True Then
                            If command.Length >= 4 Then
                                If command.Substring(0, 4) = "else" Then
                                    outcome = False
                                    checkcustomlines = False
                                ElseIf command.Substring(0, 4) = "elif" Then

                                    outcome = False
                                    checkcustomlines = False
                                Else
                                    runCommands(command, oldcommand, i + cladd)
                                End If
                            Else
                                runCommands(command, oldcommand, i + cladd)
                            End If


                        End If
                        If outcome = False Then

                            If command.Length >= 4 Then
                                If command.Substring(0, 4) = "else" Then

                                    If outcome = True Then
                                        outcome = False
                                        checkcustomlines = False
                                    End If
                                    If outcome = False Then
                                        outcome = True
                                    End If
                                ElseIf command.Substring(0, 4) = "elif" Then

                                    If outcome = True Then
                                        outcome = False
                                        checkcustomlines = False
                                    End If
                                    If outcome = False Then
                                        elif(command)

                                    End If

                                End If
                            End If

                        Else

                        End If
                    End If
                    If _for Then
                        For retnum As Integer = firstint To secondint Step intstep

                            intstore(forvarname) = retnum
                            runCommands(command, oldcommand, i + cladd)
                        Next
                    End If

                    Else

                        checkcustomlines = False
                    repeat = False
                    _if = False
                    _for = False
                    outcome = False
                    checkelse = False
                    cladd = 0
                    End If


            Loop

        Next


    End Sub

    Sub runCommands(ByVal command As String, oldcommand As String, i As Integer)
        Dim cmdlen As Integer = command.Length()

        If showcommand Then
            Console.WriteLine(command)
        End If

        If cmdlen = 0 Then
        ElseIf command.Substring(0, 1) = " " Then
        ElseIf command.Substring(0, 2) = "//" Then
        ElseIf command.Substring(0, 2) = "do" Then

        ElseIf command.Substring(0, 2) = "if" Then
            ' if(5>2):
            ' if(&num<5):
            Dim comp1 As Integer
            Dim comp2 As Integer
            Dim closebrac As Integer
            Dim sep As Integer
            Dim morethan As Boolean = False
            Dim lessthan As Boolean = False
            Dim equals As Boolean = False
            outcome = False

            For n As Integer = 1 To command.Length - 1 Step 1
                If command.Substring(n, 1) = ">" Then
                    sep = n
                    morethan = True
                ElseIf command.Substring(n, 1) = "<" Then
                    sep = n
                    lessthan = True
                ElseIf command.Substring(n, 1) = "=" Then
                    sep = n
                    equals = True
                End If

                If command.Substring(n, 1) = ")" Then
                    closebrac = n
                End If
            Next

            If command.Substring(3, 1) = "&" Then
                If intstore.ContainsKey(command.Substring(4, sep - 4)) Then
                    comp1 = CInt(intstore.Item(command.Substring(4, sep - 4)))
                End If
            Else

                comp1 = command.Substring(3, sep - 3)
            End If
            If command.Substring(sep + 1, 1) = "&" Then
                If intstore.ContainsKey(command.Substring(sep + 2, closebrac - sep - 2)) Then
                    comp2 = CInt(intstore.Item(command.Substring(sep + 2, closebrac - sep - 2)))

                End If
            Else
                comp2 = command.Substring(sep + 1, closebrac - sep - 1)
            End If
            If equals Then

                If comp1 = comp2 Then
                    outcome = True
                Else
                    outcome = False
                End If
            ElseIf morethan Then
                If comp1 > comp2 Then
                    outcome = True
                Else
                    outcome = False
                End If
            ElseIf lessthan Then

                If comp1 < comp2 Then
                    outcome = True
                Else
                    outcome = False
                End If
            Else
            End If

            _if = True
            checkcustomlines = True


        ElseIf command.Substring(0, 3) = "end" Then
            Do While 0 < 1
                Console.ReadLine()
            Loop

        ElseIf command.Substring(0, 3) = "for" Then
            'for i(1,5) step(1)
            'for(i,&start,&end) step(&step)
            ' For i as Integer = start to end Step step
            Dim lastbrac As Integer
            Dim firstbrac As Integer
            Dim sep As Integer
            Dim sfirstint As String
            Dim ssecondint As String
            Dim sintstep As String

            For n As Integer = 2 To command.Length - 1
                If command.Substring(n, 1) = ")" Then
                    lastbrac = n
                    Exit For
                End If
            Next
            For y As Integer = 2 To command.Length - 1
                If command.Substring(y, 1) = "," Then
                    sep = y
                End If
            Next
            For z As Integer = 2 To command.Length - 1
                If command.Substring(z, 1) = "(" Then
                    firstbrac = z
                    Exit For
                End If
            Next
            forvarname = command.Substring(4, firstbrac - 4)
            sfirstint = command.Substring(firstbrac + 1, sep - firstbrac - 1)
            ssecondint = command.Substring(sep + 1, lastbrac - sep - 1)
            sintstep = command.Substring(lastbrac + 7, command.Length - lastbrac - 8)
            If sfirstint.Substring(0, 1) = "&" Then
                sfirstint = sfirstint.ToString.Substring(1, sfirstint.Length - 1)
                If intstore.ContainsKey(sfirstint) Then
                    firstint = intstore.Item(sfirstint)
                End If
            Else
                firstint = CInt(sfirstint)
            End If
            If ssecondint.Substring(0, 1) = "&" Then
                ssecondint = ssecondint.ToString.Substring(1, ssecondint.Length - 1)
                If intstore.ContainsKey(ssecondint) Then
                    secondint = intstore.Item(ssecondint)
                End If
            Else
                secondint = CInt(ssecondint)
            End If
            If sintstep.Substring(0, 1) = "&" Then
                sintstep = sintstep.ToString.Substring(1, sintstep.Length - 1)
                If intstore.ContainsKey(sintstep) Then
                    intstep = intstore.Item(sintstep)
                End If
            Else
                intstep = CInt(sintstep)
            End If
            If intstore.ContainsKey(forvarname) Then
            Else
                intstore.Add(forvarname, 0)
            End If
            _for = True
            checkcustomlines = True

        ElseIf command.Substring(0, 3) = "add" Then
            Dim brac1 As Integer
            Dim brac2 As Integer
            Dim sep As Integer
            Dim x As String
            Dim y As String
            For pointer As Integer = 2 To cmdlen Step 1
                If command.Substring(pointer, 1) = "(" Then
                    brac1 = pointer
                ElseIf command.Substring(pointer, 1) = ")" Then
                    brac2 = pointer
                    Exit For
                End If
            Next
            For pointer As Integer = brac1 To brac2 Step 1
                If command.Substring(pointer, 1) = "," Then
                    sep = pointer
                End If
            Next
            x = command.Substring(brac1 + 1, sep - brac1 - 1)
            y = command.Substring(sep + 1, brac2 - sep - 1)
            If x.Substring(0, 1) = "&" Then
                x = x.Substring(1, x.Length - 1)
                If intstore.ContainsKey(x) Then
                    x = CInt(intstore.Item(x))
                End If
            End If
            If y.Substring(0, 1) = "&" Then
                y = y.Substring(1, y.Length - 1)
                If intstore.ContainsKey(y) Then
                    y = CInt(intstore.Item(y))
                End If
            End If
            add(x, y)

        ElseIf command.Substring(0, 4) = "else" Then

        ElseIf command.Substring(0, 5) = "alert" Then
            Dim message As String
            message = oldcommand.Substring(7, command.Length - 8)
            MessageBox.Show(message, setalert)


        ElseIf command.Substring(0, 5) = "write" Then
            Dim message As String = ""

            If command.Substring(command.Length - 1, 1) = "," Then
                Dim lastchar As Integer = (command.Length - 3)
                message = oldcommand.Substring(7, lastchar - 7)
                writeonline(message)
            Else
                Dim lastchar As Integer = (command.Length - 2)
                message = oldcommand.Substring(7, lastchar - 7)
                write(message)
            End If

        ElseIf command.Substring(0, 5) = "clear" Then
            Console.Clear()


        ElseIf command.Substring(0, 6) = "divide" Then
            Dim brac1 As Integer
            Dim brac2 As Integer
            Dim sep As Integer
            Dim x As String
            Dim y As String



            For pointer As Integer = 5 To cmdlen Step 1
                If command.Substring(pointer, 1) = "(" Then
                    brac1 = pointer
                ElseIf command.Substring(pointer, 1) = ")" Then
                    brac2 = pointer
                    Exit For
                End If
            Next
            For pointer As Integer = brac1 To brac2 Step 1
                If command.Substring(pointer, 1) = "," Then
                    sep = pointer
                End If
            Next
            x = command.Substring(brac1 + 1, sep - brac1 - 1)
            y = command.Substring(sep + 1, brac2 - sep - 1)
            If x.Substring(0, 1) = "&" Then
                x = x.Substring(1, x.Length - 1)
                If intstore.ContainsKey(x) Then
                    x = CInt(intstore.Item(x))
                End If
            End If
            If y.Substring(0, 1) = "&" Then
                y = y.Substring(1, y.Length - 1)
                If intstore.ContainsKey(y) Then
                    y = CInt(intstore.Item(y))
                End If
            End If

            divide(x, y)
        ElseIf command.Substring(0, 6) = "repeat" Then
            Dim closebrac As Integer
            Dim times As Integer
            For n As Integer = 4 To command.Length - 1
                If command.Substring(n, 1) = ")" Then
                    closebrac = n
                End If
            Next

            If command.Substring(7, 1) = "&" Then
                If intstore.ContainsKey(command.Substring(8, closebrac - 8)) Then
                    times = CInt(intstore.Item(command.Substring(8, closebrac - 8)))
                End If
            Else
                times = CInt(command.Substring(7, closebrac - 7))
            End If

            repeat = True
            repetitions = times
            checkcustomlines = True
        ElseIf command.Substring(0, 7) = "set int" Then
            ' set int(int name,value)
            ' set int(int name,&variable value)
            Dim sep As Integer = 0
            Dim lastbrac As Integer = 0
            Dim varname As String = ""
            Dim variablename As String = ""
            Dim varvalue As Integer = 0

            For n As Integer = 6 To command.Length - 1 Step 1
                If command.Substring(n, 1) = "," Then
                    sep = n
                ElseIf command.Substring(n, 1) = ")" Then
                    lastbrac = n
                End If
            Next
            varname = command.Substring(8, sep - 8)
            If command.Substring(sep + 1, 1) = "&" Then
                variablename = command.Substring(sep + 2, command.Length - sep - 3)

                If intstore.ContainsKey(variablename) Then

                    varvalue = intstore.Item(variablename)
                Else
                End If

            Else
                Console.WriteLine(variablename)
                varvalue = command.Substring(sep + 1, command.Length - sep - 2)
            End If
            If intstore.ContainsKey(varname) Then
                intstore(varname) = varvalue
            Else
                intstore.Add(varname, varvalue)
            End If


        ElseIf command.Substring(0, 8) = "subtract" Then
            Dim brac1 As Integer
            Dim brac2 As Integer
            Dim sep As Integer
            Dim x As String
            Dim y As String
            For pointer As Integer = 6 To cmdlen Step 1
                If command.Substring(pointer, 1) = "(" Then
                    brac1 = pointer
                ElseIf command.Substring(pointer, 1) = ")" Then
                    brac2 = pointer
                    Exit For
                End If
            Next
            For pointer As Integer = brac1 To brac2 Step 1
                If command.Substring(pointer, 1) = "," Then
                    sep = pointer
                End If
            Next
            x = command.Substring(brac1 + 1, sep - brac1 - 1)
            y = command.Substring(sep + 1, brac2 - sep - 1)
            If x.Substring(0, 1) = "&" Then
                x = x.Substring(1, x.Length - 1)
                If intstore.ContainsKey(x) Then
                    x = CInt(intstore.Item(x))
                End If
            End If
            If y.Substring(0, 1) = "&" Then
                y = y.Substring(1, y.Length - 1)
                If intstore.ContainsKey(y) Then
                    y = CInt(intstore.Item(y))
                End If
            End If

            subtract(x, y)

        ElseIf command.Substring(0, 8) = "setalert" Then
            Dim message As String
            message = oldcommand.Substring(10, command.Length - 11)
            setalert = message

        ElseIf command.Substring(0, 8) = "multiply" Then
            Dim brac1 As Integer
            Dim brac2 As Integer
            Dim sep As Integer
            Dim x As String
            Dim y As String
            For pointer As Integer = 6 To cmdlen Step 1
                If command.Substring(pointer, 1) = "(" Then
                    brac1 = pointer
                ElseIf command.Substring(pointer, 1) = ")" Then
                    brac2 = pointer
                    Exit For
                End If
            Next
            For pointer As Integer = brac1 To brac2 Step 1
                If command.Substring(pointer, 1) = "," Then
                    sep = pointer
                End If
            Next
            x = command.Substring(brac1 + 1, sep - brac1 - 1)
            y = command.Substring(sep + 1, brac2 - sep - 1)
            If x.Substring(0, 1) = "&" Then
                x = x.Substring(1, x.Length - 1)
                If intstore.ContainsKey(x) Then
                    x = CInt(intstore.Item(x))
                End If
            End If
            If y.Substring(0, 1) = "&" Then
                y = y.Substring(1, y.Length - 1)
                If intstore.ContainsKey(y) Then
                    y = CInt(intstore.Item(y))
                End If
            End If

            multiply(x, y)
        ElseIf command.Substring(0, 9) = "in colour" Then
            Dim colourname As String
            colourname = command.Substring(10, (command.Length - 1) - 10)
            Select Case colourname
                Case "red"
                    Console.ForegroundColor = ConsoleColor.Red
                Case "black"
                    Console.ForegroundColor = ConsoleColor.Black
                Case "yellow"
                    Console.ForegroundColor = ConsoleColor.Yellow
                Case "pink"
                    Console.ForegroundColor = ConsoleColor.Magenta
                Case "green"
                    Console.ForegroundColor = ConsoleColor.Green
                Case "orange"
                    Console.ForegroundColor = ConsoleColor.DarkYellow
                Case "purple"
                    Console.ForegroundColor = ConsoleColor.DarkMagenta
                Case "blue"
                    Console.ForegroundColor = ConsoleColor.Blue
                Case "white"
                    Console.ForegroundColor = ConsoleColor.White
                Case "grey"
                    Console.ForegroundColor = ConsoleColor.Gray
                Case Else
                    Console.ForegroundColor = ConsoleColor.Gray
            End Select
        ElseIf command.Substring(0, 9) = "input int" Then
            Dim variablename As String
            Dim variablevalue As Integer
            variablename = command.Substring(10, command.Length - 11)
            variablevalue = Console.ReadLine()
            intstore.Add(variablename, variablevalue)

        ElseIf command.Substring(0, 10) = "return int" Then
            Dim variablename As String
            Dim variablevalue As Integer
            If command.Substring(command.Length - 1, 1) = "," Then
                variablename = command.Substring(11, command.Length - 13)
                If intstore.ContainsKey(variablename) Then
                    variablevalue = intstore.Item(variablename)
                End If
                Console.Write(variablevalue)
            Else
                variablename = command.Substring(11, command.Length - 12)
                If intstore.ContainsKey(variablename) Then
                    variablevalue = intstore.Item(variablename)
                End If
                Console.WriteLine(variablevalue)
            End If

        ElseIf command.Substring(0, 10) = "dump input" Then
            If command.Substring(command.Length - 1, 1) = "," Then
                Console.Write(tempstorage)
            Else
                Console.WriteLine(tempstorage)
            End If
        ElseIf command.Substring(0, 11) = "store input" Then
            If command.Substring(command.Length - 1, 1) = "," Then
                tempstorage = Console.Read()
            Else
                tempstorage = Console.ReadLine()
            End If
        ElseIf command.Substring(0, 12) = "show command" Then
            If showcommand = False Then
                showcommand = True
            ElseIf showcommand = True Then
                showcommand = False
            End If

        ElseIf command.Substring(0, 12) = "input string" Then
            Dim variablename As String
            Dim variablevalue As String

            variablename = command.Substring(13, command.Length - 14)
            variablevalue = Console.ReadLine()
            stringstore.Add(variablename, variablevalue)

        ElseIf command.Substring(0, 13) = "return string" Then
            Dim variablename As String
            Dim variablevalue As String
            If command.Substring(command.Length - 1, 1) = "," Then
                variablename = command.Substring(14, command.Length - 16)
                If stringstore.ContainsKey(variablename) Then
                    variablevalue = stringstore.Item(variablename)
                End If
                Console.Write(variablevalue)
            Else
                variablename = command.Substring(14, command.Length - 15)
                If stringstore.ContainsKey(variablename) Then
                    variablevalue = stringstore.Item(variablename)
                End If
                Console.WriteLine(variablevalue)
            End If

        Else

        End If
    End Sub

    Sub add(ByVal x As Integer, y As Integer)
        Console.WriteLine(x + y)
    End Sub

    Sub multiply(ByVal x As Integer, y As Integer)
        Console.WriteLine(x * y)
    End Sub

    Sub subtract(ByVal x As Integer, y As Integer)
        Console.WriteLine(x - y)
    End Sub

    Sub divide(ByVal x As Integer, y As Integer)
        Console.WriteLine(x / y)
    End Sub

    Sub write(ByVal text)
        Console.WriteLine(text)
    End Sub

    Sub writeonline(ByVal text)
        Console.Write(text)
    End Sub

    Sub elif(ByVal command As String)
        command = command.Substring(2, command.Length - 2)
        Dim comp1 As Integer
        Dim comp2 As Integer
        Dim closebrac As Integer
        Dim sep As Integer
        Dim morethan As Boolean = False
        Dim lessthan As Boolean = False
        Dim equals As Boolean = False
        outcome = False

        For n As Integer = 1 To command.Length - 1 Step 1
            If command.Substring(n, 1) = ">" Then
                sep = n
                morethan = True
            ElseIf command.Substring(n, 1) = "<" Then
                sep = n
                lessthan = True
            ElseIf command.Substring(n, 1) = "=" Then
                sep = n
                equals = True
            End If

            If command.Substring(n, 1) = ")" Then
                closebrac = n
            End If
        Next

        If command.Substring(3, 1) = "&" Then
            If intstore.ContainsKey(command.Substring(4, sep - 4)) Then
                comp1 = CInt(intstore.Item(command.Substring(4, sep - 4)))
            End If
        Else
            comp1 = command.Substring(3, sep - 3)
        End If
        If command.Substring(sep + 1, 1) = "&" Then
            If intstore.ContainsKey(command.Substring(sep + 2, closebrac - sep - 2)) Then
                comp2 = CInt(intstore.Item(command.Substring(sep + 2, closebrac - sep - 2)))

            End If
        Else
            comp2 = command.Substring(sep + 1, closebrac - sep - 1)
        End If
        If equals Then

            If comp1 = comp2 Then
                outcome = True
            Else
                outcome = False
            End If
        ElseIf morethan Then
            If comp1 > comp2 Then
                outcome = True
            Else
                outcome = False
            End If
        ElseIf lessthan Then

            If comp1 < comp2 Then
                outcome = True
            Else
                outcome = False
            End If
        Else
        End If
    End Sub

End Module
