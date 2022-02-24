exception DivisionByZero; (*Exception when division by zero*)
exception IndexError; (*Exception when a negative or invalid index is used*)
exception booleanDomain; (*Exception when and or not are performed on non boolean quantities*)

(*Function for character at any index of string*)
fun indexer(strin,i)=
    if i>size(strin)-1 then chr(0) else
    String.sub(strin,i);



(*Function to convert an input line to a string with spaces between integers*)
(*A string with space is created to directly utilize the splitting by
space function of sml in next function*)
fun converter1(instream,i)=
    let 
        val newstring=""
        val chara = indexer(instream,i)
        val temp=String.str(chara)
    in
        if i=size(instream) then "" else
        if (temp="\n" orelse temp="(" orelse temp=")") then converter1(instream,i+1) else
        if temp="," then newstring^" "^converter1(instream,i+1) else 
        newstring^temp^converter1(instream,i+1)
    end;



(*Function to convert a string with spaces into a list of tuples*)
fun tuple(spaced)=
    let 
        val stri=String.tokens Char.isSpace spaced
        fun integer_of(element)=
            valOf(Int.fromString(element));
    in
        if spaced="" then [] else
        [(integer_of(hd(stri)),integer_of(hd(tl(stri))),integer_of(hd(tl(tl(stri)))),integer_of(hd(tl(tl(tl(stri))))))]
    end;



(*Function to read file and convert in vector of tuples*)
(*The inputLine function of TextIO is used to convert each line into tuple
and then appending it recursively to next lines*)
(*A list has been created and then converted into vector*)
fun readlist (infile : string) = let 
  val ins = TextIO.openIn infile 
  fun cover(ins) = 
   case TextIO.inputLine ins of 
      SOME line => tuple(converter1(line,0))@cover(ins)
    | NONE      => [] 
in 
  Vector.fromList(cover(ins)) before TextIO.closeIn ins
end ;


(*This function directly returns the ith element of array*)
(*It has been created separately to handle errors in case of negative indices*)
fun at_index_array(arr,i)=
    if i<0 then raise IndexError else
    Array.sub(arr,i);

(*This function directly returns the ith element of vector*)
(*It has been created separately to handle errors in case of negative indices*)
fun at_index_vector(vec,i)=
    if i<0 then raise IndexError else
    Vector.sub(vec,i);


(*This function returns the array updated at the ith element with value j*)
(*It raises error for negative index*)
fun update_at_index(a,i,j)=
    if i<0 then raise IndexError else
    let 
        val unit = Array.update(a,i,j)
    in
        a
    end;


(*This function converts a boolean value in 0 or 1 appropriately*)
fun bool_convertor(x)=
    if x=true then 1 else 0;

(*This function gives an integer after taking input from the user*)
fun input_taker(m)=
    let
        val x=print("input:")
        val str = valOf (TextIO.inputLine TextIO.stdIn)
        val i : int = valOf (Int.fromString str)
    in
        i 
    end;

(*This function is for 'and' of boolean values*)
fun boolean_and(x,y)=
    if ((x=1 orelse x=0) andalso (y=0 orelse y=1)) then x*y else
    raise booleanDomain;

(*This function is for 'not' of boolean value*)
fun boolean_not(x)=
    if x=0 then 1 else
    if x=1 then 0 else
    raise booleanDomain;

(*This function is for 'or' of boolean values*)
fun boolean_or(x,y)=
    if x=0 andalso y=0 then 0 else
    if x=0 andalso y=1 then 1 else
    if x=1 andalso y=0 then 1 else
    if x=1 andalso y=1 then 1 else
    raise booleanDomain;



(*This function updates the memory acoording to the tuple.
When x=13,14 or 15 then there is no need to update the memory therefore the last else does nothing but
returns the same array*)
(*It would raise index error if x<0 or >16*)
fun update(memory,(x,y,z,w))=
    if x=1 then update_at_index(memory,w,input_taker()) else
    if x=2 then update_at_index(memory,w,at_index_array(memory,y)) else
    if x=3 then update_at_index(memory,w,boolean_not(at_index_array(memory,y))) else
    if x=4 then update_at_index(memory,w,boolean_or(at_index_array(memory,y),at_index_array(memory,z))) else
    if x=5 then update_at_index(memory,w,boolean_and(at_index_array(memory,y),at_index_array(memory,z))) else
    if x=6 then update_at_index(memory,w,at_index_array(memory,y)+at_index_array(memory,z)) else
    if x=7 then update_at_index(memory,w,at_index_array(memory,y)-at_index_array(memory,z)) else
    if x=8 then update_at_index(memory,w,at_index_array(memory,y)*at_index_array(memory,z)) else
    if x=9 then
            if at_index_array(memory,z)=0 then raise DivisionByZero else
            update_at_index(memory,w,at_index_array(memory,y) div at_index_array(memory,z)) 
    else
    if x=10 then
            if at_index_array(memory,z)=0 then raise DivisionByZero else
            update_at_index(memory,w,at_index_array(memory,y) mod at_index_array(memory,z)) 
    else
    if x=11 then update_at_index(memory,w,bool_convertor(at_index_array(memory,y) = at_index_array(memory,z))) else
    if x=12 then update_at_index(memory,w,bool_convertor(at_index_array(memory,y) > at_index_array(memory,z))) else
    if x=16 then update_at_index(memory,w,y) else
    if (x=13 orelse x=14 orelse x=15) then update_at_index(memory,0,at_index_array(memory,0)) else 
    raise IndexError;


(*This function helps locate differently indexed values of tuples*)
fun tuple_loc((x,y,z,w),i)=
    if i=1 then x else
    if i=2 then y else
    if i=3 then z else
    if i=4 then w else
    raise Fail("Invalid input");

(*This function updates memory and outputs needed things as per the input vector*)
(*It uses an iterative algorithm and here i is the index indicating where in the code,
the function is working currently*)
(*The helper function stringer helps print the output when code[c]'s first element is 15
and then proceeds further. In case of quadruple's first element as 13 or 14, the function 
moves to appropriate line of the code as per the condition provided by other indices.*)
(*The algorithm prints execution halted when it encounters a zero
and terminates with a unit value when lines to read are over*)
(*Exceptions have been handles in this function since it is of unit return type*)
fun interpreter(v,memory,i)=
    let
        fun stringer(memory,v,i)=
                let         
                    val str = print(Int.toString(at_index_array(memory,tuple_loc(at_index_vector(v,i),2)))^"\n")
                in 
                    interpreter(v,memory,i+1)
                end; 
    in
        (if i=Vector.length(v) then () else
        if tuple_loc(at_index_vector(v,i),1)=0 then () else
        if tuple_loc(at_index_vector(v,i),1)=15 then stringer(memory,v,i) else
        if (tuple_loc(at_index_vector(v,i),1)=13 andalso at_index_array(memory,tuple_loc(at_index_vector(v,i),2))=1) 
            then interpreter(v,memory,tuple_loc(at_index_vector(v,i),4)) else
        if tuple_loc(at_index_vector(v,i),1)=14  then 
            interpreter(v,memory,tuple_loc(at_index_vector(v,i),4)) else
        interpreter(v,update(memory,at_index_vector(v,i)),i+1))
        handle DivisionByZero => print(("Division by zero in line "^Int.toString(i+1))^"\n")
               | IndexError => print(("Invalid index used in line "^Int.toString(i+1))^"\n")
               | booleanDomain => print(("Boolean expression used on non boolean values in line "^Int.toString(i+1))^"\n")
    end;

    
(*This function takes a string name of file and interprets it*)
(*The memory intiated in this function is an array of size 1000*)
fun interpret(x)=
    let val list=readlist(x) 
    val a = Array.array(1000,0);
    in
    interpreter(list,a,0)
    end;


        
















    








    







