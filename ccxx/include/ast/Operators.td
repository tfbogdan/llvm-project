

class Operator<string spelling, int precedence> {
    string Spelling = spelling;
    int Precedence = precedence;
}

class BinaryOperator<string spelling, int precedence, bit leftAssociative = 1> : Operator<spelling, precedence> {
    bit LeftAssociative = leftAssociative;
}

class UnaryOperator<string spelling, int precedence> : Operator<spelling, precedence>;

def Add         : BinaryOperator<"+", 5>;
def Substract   : BinaryOperator<"-", 5>;
def Multiply    : BinaryOperator<"*", 6>;
def Divide      : BinaryOperator<"/", 6>;

// def IntNegate   : UnaryOperator<"-", 7>;