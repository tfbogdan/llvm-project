```C++
// Line comment

/* Block comment */

// constant definition
let someName :: int = 2

// variable definition
var someVarName :: int = 3

// generic function. a's type is unspecified and so is the return type
func square(a): 
    a * a

// Formally, a function definition follows the following pattern:
[pure|safe|inline|public|private|whatever] func name(argName :: argType = defaultArgValue, args...) -> returnType throws ExceptionType1, ExceptionType2 functionBody 

// ;

class mixin valueProperty<fieldName, propertyName> {
    property #propertyName {
        let fieldType = typeof #fieldName
        public get -> fieldType: #fieldName

        public set(Value :: fieldType) {
            #fieldName = Value
        }
    }
}

class Vector2<T> {
    // when access level is omitted inside classes, it defaults to the most restrictive, ie private
    data _x :: T
    data _y :: T

    valueProperty<_x, x>
    valueProperty<_y, y>
            
    public func setX( X ): _x = X
}

func mixin sumOfSquares<args...>: (square args.head) + sumOfSquares<args.tail>
func mixin sumOfSquares<>: 0
func mixin sumOfSquares<arg>: square arg

class Vector3<T> {
    data _x :: T
    data _y :: T
    data _z :: T

    valueProperty<_x, x>
    valueProperty<_y, y>
    valueProperty<_z, z>

    property xy {
        public get = Vector2<T> { _x, _y }
        public set(XY: &Vector2<T>) {
            _x = XY.x
            _y = XY.y
        }
    }

    func magnitude() = sqrt sumOfSquares<_x, _y, _z>
}


class vector<ElemType :: type> {
    data _data :: *ElemType = nullptr
    data _size :: unsigned = 0
}

class array<T :: type, Count :: unsigned> {
    data _data :: [T, Count] = { T{}... }
    
    public op[] (index :: unsigned) -> &T = _data[index]
    public func front -> &T = _data[index]
    public func back -> &T = _data[Count - 1]

    public func begin = &_data[0]
    public func end = begin + Count

    public func size = Count
}


struct FieldData {
    data name :: identifier
    data type :: identifier
}

/** Functions may be evaluated at compile time if they refrain from doing a few specific things:
    - Allocating memory
    - Calling function pointers

    Metaprogramming is done using macros, which resemble functions very much. Unlike functions, their evaluation is guaranteed to occur at compile time. In addition to functions, they can perform introspection. Macros are compiled to native code before execution, for the host platform ABI. During cross compilation, the host and target ABIs may differ. That means that they should be used for metaprogramming mostly and not for precomputing runtime data. For that purpose, regular functions can also be called at compile time. Unlike macros, their execution will be performed more like an interpreted language, without precompilation.

    Mixins allow programatically inserting blobs of code into a context, such as a class, a function, a namespace. 

    Declaring them resembles functions too, just like macros. Their evaluation, however, has the effect of emplacing their body into their calling context, after performing whatever substitutions they may have to. One of their special powers is the ability to turn regular strings known at compile time into identifiers by wrapping them into <>.
*/

// -- All that follows is obsoleted by the above
class mixin insertPublicField<FieldName :: identifier, FieldType :: identifier> {
    public #FieldName: typeof #FieldType;
}

class macro insertField<field :: FieldData> {
    if (field.fName != nullptr and field.fType != nullptr) {
        insertPublicField<field.fName, field.fType>;
    }
}

class mixin value_type {
    public constructor() = default;
    public constructor(copy) = default;
    public constructor(move) = default; 
}

struct GeneratedType<fields: [FieldData]> {
    // Control flow expressions at the top level of a template are simply evaluated at compile time, for every template instantiation
    for (f in fields) {
        insertField();
    }
    // Macro calls at the top level are also evaluated immediately, once per instantiation
    mixin value_type();
};

```

