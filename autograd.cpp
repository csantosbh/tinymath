#include <iostream>
#include <cassert>
#include <deque>
#include <array>
#include <string>
#include <sstream>
#include <memory>
#include <cmath>
#include <functional>
#include <type_traits>

using namespace std;

class Expression;
class Expr: public shared_ptr<Expression>
{
public:
    Expr() : shared_ptr<Expression>() {}
    Expr(Expression* ptr): shared_ptr<Expression>(ptr) {}
    Expr(double value);
    Expr(int value);
    Expr(double value, string name);
};

template<typename Type, typename ...ParamTypes>
Expr make_expr(ParamTypes&& ...params)
{
    // TODO properly implement this method
    return Expr(new Type(std::forward<ParamTypes>(params)...));
}

class Expression
{
    protected:
        enum class ExprClass {Variable, Constant};

    public:
        Expression() {}

        Expression(const initializer_list<Expr>& inputs_)
            : inputs_(inputs_) {}

        virtual ~Expression () {}

        virtual Expr derivative(const Expr& target) const = 0;

        virtual double value() const = 0;

        virtual operator string() const = 0;

        virtual Expr simplify() const = 0;

        virtual ExprClass expressionClass() const {
            for(const auto& operand: inputs_) {
                if(operand->expressionClass() == ExprClass::Variable) {
                    return ExprClass::Variable;
                }
            }

            return ExprClass::Constant;
        }

    protected:
        template<typename ExprType>
        Expr unaryOperatorSimplify() const;

        template<typename ExprType>
        Expr binaryOperatorSimplify(
                const function<Expr(const Expr&, double)>& aConstantCbk,
                const function<Expr(const Expr&, double)>& bConstantCbk) const;

        deque<Expr> inputs_;

        string binaryOperatorToString(const string& opString,
                                      bool parenthesize = true) const;
        string unaryOperatorToString(const string& opString,
                                     bool parenthesize = true) const;
};

class Constant: public Expression
{
    public:
        Constant(double value) : value_(value) { }

        virtual Expr derivative(const Expr&) const {
            return Expr(0);//make_shared<Constant>(0);
        }

        virtual double value() const {
            return value_;
        }

        virtual operator string() const {
            return (stringstream() << value_).str();
        }

        virtual Expr simplify() const {
            return Expr(value_);//make_shared<Constant>(value_);
        }

        virtual ExprClass expressionClass() const {
            return ExprClass::Constant;
        }

    private:
        double value_;
};

class Variable: public Expression
{
    public:
    // TODO check if name is repeated
        Variable(double value,
                const string& name)
            : value_(value)
              , name_(name) { }

        void setValue(double v) {
            value_ = v;
        }

        virtual Expr derivative(const Expr& expr) const;

        virtual operator string() const {
            return name_;
        }

        virtual double value() const {
            return value_;
        }

        virtual Expr simplify() const {
            return Expr(value_, name_);//make_shared<Variable>(value_, name_);
        }

        virtual ExprClass expressionClass() const {
            return ExprClass::Variable;
        }

    private:
        double value_;
        string name_;
};

Expr operator-(Expr a);
Expr operator+(Expr a, Expr b);
Expr operator-(Expr a, Expr b);
Expr operator*(Expr a, Expr b);
Expr operator/(Expr a, Expr b);

#define MK_OPERATION_CONSTRUCTOR(OpClass) \
    public: \
    OpClass() {} \
    OpClass(const initializer_list<Expr>& inputs_) \
   : Expression(inputs_) {} \
    OpClass& operator=(OpClass&){return *this;}\

#define MK_FUNCTION_CONSTRUCTOR(OpClass) \
    public: \
    OpClass(const initializer_list<Expr>& inputs_) \
   : Function(inputs_) {}


class SignInversion: public Expression
{
    public:
        MK_OPERATION_CONSTRUCTOR(SignInversion)

        virtual double value() const {
            return -inputs_[0]->value();
        }

        virtual operator string() const {
            return unaryOperatorToString("-");
        }

        virtual Expr derivative(const Expr& target) const {
            return -inputs_[0]->derivative(target);
        }

        virtual Expr simplify() const {
            return unaryOperatorSimplify<SignInversion>();
        }
};

Expr operator-(Expr a) {
    return make_expr<SignInversion>(initializer_list<Expr>{a});
}

class Addition: public Expression
{
    MK_OPERATION_CONSTRUCTOR(Addition)

    public:
        virtual double value() const {
            return inputs_[0]->value() + inputs_[1]->value();
        }

        virtual operator string() const {
            return binaryOperatorToString(" + ");
        }

        virtual Expr derivative(const Expr& target) const {
            return inputs_[0]->derivative(target) +
                   inputs_[1]->derivative(target);
        }

        virtual Expr simplify() const {
            const auto oneConstantCbk =
            [](const Expr& variable, double constant) -> Expr {
                if(constant == 0) return variable;
                return make_expr<Constant>(constant) + variable;
            };

            return binaryOperatorSimplify<Addition>(
                oneConstantCbk, oneConstantCbk);
        }
};

Expr operator+(Expr a, Expr b) {
    return make_expr<Addition>(initializer_list<Expr>{a, b});
}

class Subtraction: public Expression
{
    MK_OPERATION_CONSTRUCTOR(Subtraction)

    public:
        virtual double value() const {
            return inputs_[0]->value() - inputs_[1]->value();
        }

        virtual operator string() const {
            return binaryOperatorToString(" - ");
        }

        virtual Expr derivative(const Expr& target) const {
            return inputs_[0]->derivative(target) - inputs_[1]->derivative(target);
        }

        virtual Expr simplify() const {
            return binaryOperatorSimplify<Subtraction>(
                [](const Expr& b, double a) -> Expr {
                    if(a == 0) return b;
                    return make_expr<Constant>(a) - b;
                },
                [](const Expr& a, double b) -> Expr {
                    if(b == 0) return a;
                    return a - make_expr<Constant>(b);
                }
            );
        }
};

Expr operator-(Expr a, Expr b) {
    return make_expr<Subtraction>(initializer_list<Expr>{a, b});
}

class Multiplication: public Expression
{
    MK_OPERATION_CONSTRUCTOR(Multiplication)

    public:
        virtual double value() const {
            return inputs_[0]->value() * inputs_[1]->value();
        }

        virtual operator string() const {
            return binaryOperatorToString("*");
        }

        virtual Expr derivative(const Expr& target) const {
            return inputs_[0]->derivative(target) * inputs_[1] +
                inputs_[0] * inputs_[1]->derivative(target);
        }

        virtual Expr simplify() const {
            const auto oneConstantCbk =
            [](const Expr& variable, double constant) -> Expr {
                if(constant == 0) return 0.0;
                if(constant == 1) return variable;

                return make_expr<Constant>(constant) * variable;
            };

            return binaryOperatorSimplify<Multiplication>(
                oneConstantCbk, oneConstantCbk);
        }
};

Expr operator*(Expr a, Expr b) {
    return make_expr<Multiplication>(initializer_list<Expr>{a, b});
}

class Division: public Expression
{
    MK_OPERATION_CONSTRUCTOR(Division)

    public:
        virtual double value() const {
            return inputs_[0]->value() / inputs_[1]->value();
        }

        virtual operator string() const {
            return binaryOperatorToString("/");
        }

        virtual Expr derivative(const Expr& target) const {
            return (inputs_[0]->derivative(target) * inputs_[1] -
                    inputs_[0] * inputs_[1]->derivative(target)) /
                (inputs_[1] * inputs_[1]);
        }

        virtual Expr simplify() const {
            return binaryOperatorSimplify<Division>(
                [](const Expr& b, double a) -> Expr {
                    if(a == 0) return 0.0;
                    return make_expr<Constant>(a) / b;
                },
                [](const Expr& a, double b) -> Expr {
                    if(b == 0) return 
                            make_expr<Constant>(
                                std::numeric_limits<double>::quiet_NaN());
                    if(b == 1) return a;

                    return a / make_expr<Constant>(b);
                }
            );
        }
};

Expr operator/(Expr a, Expr b) {
    return make_expr<Division>(initializer_list<Expr>{a, b});
}

class Function: public Expression
{
    MK_OPERATION_CONSTRUCTOR(Function)

    public:
        ~Function() {}

        virtual Expr fnDerivative(const Expr& target) const = 0;

        virtual Expr derivative(const Expr& target) const {
            // Chain rule: df(g1(x), g2(x), ...) = sum{df(g1, g2, ...)*dg_n}
            auto result = fnDerivative(inputs_[0]) *
                inputs_[0]->derivative(target);
            for(int i = 1; i < inputs_.size(); ++i) {
                result = result + fnDerivative(inputs_[i]) *
                    inputs_[i]->derivative(target);
            }
            return result;
        }

    protected:
        string fnOperatorToString(const string& fnName) const {
            stringstream res;

            res << fnName << "(";
            for(int i = 0; i < inputs_.size(); ++i) {
                res << static_cast<string>(*inputs_[i]);
                if(i < inputs_.size() - 1) {
                    res << ", ";
                }
            }
            res << ")";

            return res.str();
        }
};

template<typename ExprType>
Expr Expression::unaryOperatorSimplify() const {
    auto aExpr = inputs_[0]->simplify();

    const auto aClass = aExpr->expressionClass();

    const auto aVal = aExpr->value();

    const bool isConstant = (aClass == ExprClass::Constant);

    if(isConstant) {
        return make_expr<Constant>(this->value());
    } else {
        return make_expr<ExprType>(
                initializer_list<Expr>{aExpr});
    }
}

template<typename ExprType>
Expr Expression::binaryOperatorSimplify(
        const function<Expr(const Expr&, double)>& aConstantCbk,
        const function<Expr(const Expr&, double)>& bConstantCbk) const {
    auto aExpr = inputs_[0]->simplify();
    auto bExpr = inputs_[1]->simplify();

    const auto aClass = aExpr->expressionClass();
    const auto bClass = bExpr->expressionClass();

    const auto aVal = aExpr->value();
    const auto bVal = bExpr->value();

    const bool bothConstant =
        (aClass == ExprClass::Constant) &&
        (bClass == ExprClass::Constant);
    const bool neitherConstant =
        (aClass != ExprClass::Constant) &&
        (bClass != ExprClass::Constant);

    if(bothConstant) {
        return make_expr<Constant>(this->value());
    } else if(neitherConstant) {
        return make_expr<ExprType>(
                initializer_list<Expr>{aExpr, bExpr});
    } else {
        return bClass == ExprClass::Constant ?
            bConstantCbk(aExpr, bVal) :
            aConstantCbk(bExpr, aVal);
    }
}

Expr cos(const Expr&);
Expr sin(const Expr&);
Expr tan(const Expr&);

class Cosine: public Function
{
    MK_FUNCTION_CONSTRUCTOR(Cosine)

    public:
        virtual double value() const {
            return cos(inputs_[0]->value());
        }

        virtual operator string() const {
            return fnOperatorToString("cos");
        }

        virtual Expr fnDerivative(const Expr& target) const;

        virtual Expr simplify() const {
            return unaryOperatorSimplify<Cosine>();
        }
};

Expr cos(const Expr& val) {
    return make_expr<Cosine>(initializer_list<Expr>{val});
}

class Sine: public Function
{
    MK_FUNCTION_CONSTRUCTOR(Sine)

    public:
        virtual double value() const {
            return sin(inputs_[0]->value());
        }

        virtual operator string() const {
            return fnOperatorToString("sin");
        }

        virtual Expr fnDerivative(const Expr& target) const {
            return cos(inputs_[0]);
        }

        virtual Expr simplify() const {
            return unaryOperatorSimplify<Sine>();
        }
};

Expr sin(const Expr& val) {
    return make_expr<Sine>(initializer_list<Expr>{val});
}

class Tangent: public Function
{
    MK_FUNCTION_CONSTRUCTOR(Tangent)

    public:
        virtual double value() const {
            return tan(inputs_[0]->value());
        }

        virtual operator string() const {
            return fnOperatorToString("tan");
        }

        virtual Expr fnDerivative(const Expr& target) const {
            return 1.0/(cos(inputs_[0]) * cos(inputs_[0]));
        }

        virtual Expr simplify() const {
            return unaryOperatorSimplify<Tangent>();
        }
};

Expr tan(const Expr& val) {
    return make_expr<Tangent>(initializer_list<Expr>{val});
}

Expr log(const Expr& val);
Expr pow(const Expr& a, const Expr& b);

class LogNatural: public Function
{
    MK_FUNCTION_CONSTRUCTOR(LogNatural)

    public:
        virtual double value() const {
            return log(inputs_[0]->value());
        }

        virtual operator string() const {
            return fnOperatorToString("log");
        }

        virtual Expr fnDerivative(const Expr& target) const {
            return 1.0 / inputs_[0];
        }

        virtual Expr simplify() const {
            return unaryOperatorSimplify<LogNatural>();
        }
};

Expr log(const Expr& val) {
    return make_expr<LogNatural>(initializer_list<Expr>{val});
}

class Power: public Function
{
    MK_FUNCTION_CONSTRUCTOR(Power)

    public:
        virtual double value() const {
            return pow(inputs_[0]->value(), inputs_[1]->value());
        }

        virtual operator string() const {
            return fnOperatorToString("pow");
        }

        virtual Expr fnDerivative(const Expr& target) const {
            if(target == inputs_[0]) {
                // d x^k/dx
                return inputs_[1] * pow(inputs_[0], inputs_[1] - 1.0);
            } else {
                // d k^x/dx
                return pow(inputs_[0], inputs_[1]) * log(inputs_[0]);
            }
        }

        virtual Expr simplify() const {
            return binaryOperatorSimplify<Power>(
                [](const Expr& b, double a) -> Expr {
                    if(a == 0) return 0.0;
                    if(a == 1) return 1.0;

                    return pow(make_expr<Constant>(a), b);
                },
                [](const Expr& a, double b) -> Expr {
                    if(b == 0) return 1.0;
                    if(b == 1) return a;

                    return pow(a, make_expr<Constant>(b));
                }
            );
        }
};

Expr pow(const Expr& a, const Expr& b) {
    return make_expr<Power>(initializer_list<Expr>{a, b});
}

class Sqrt: public Function
{
    MK_FUNCTION_CONSTRUCTOR(Sqrt)

    public:
        virtual double value() const {
            return sqrt(inputs_[0]->value());
        }

        virtual operator string() const {
            return fnOperatorToString("sqrt");
        }

        virtual Expr fnDerivative(const Expr& target) const {
            return 0.5 * pow(target, -0.5);
        }

        virtual Expr simplify() const {
            return unaryOperatorSimplify<Sqrt>();
        }
};

Expr sqrt(const Expr& a) {
    return make_expr<Sqrt>(initializer_list<Expr>{a});
}

string Expression::binaryOperatorToString(const string& opString,
                                          bool parenthesize) const {
    stringstream res;

    const string openPar = parenthesize ? "(" : "";
    const string closePar = parenthesize ? ")" : "";

    res << openPar << static_cast<string>(*inputs_[0])
        << opString
        << static_cast<string>(*inputs_[1]) << closePar;
    return res.str();
}

string Expression::unaryOperatorToString(const string& opString,
                                         bool parenthesize) const {
    stringstream res;

    const string openPar = parenthesize ? "(" : "";
    const string closePar = parenthesize ? ")" : "";

    res << opString << openPar
        << static_cast<string>(*inputs_[0]) << closePar;
    return res.str();
}

Expr Cosine::fnDerivative(const Expr& target) const {
    return -sin(inputs_[0]);
}

Expr Variable::derivative(const Expr& expr) const {
    return expr.get() == this ? 1.0 : 0.0;
}

ostream& operator<<(ostream& out, const Expr& expr) {
    out << static_cast<string>(*expr);
    return out;
}

Expr::Expr(double value): shared_ptr<Expression>(new Constant(value)) { }
Expr::Expr(int value): shared_ptr<Expression>(new Constant(value)) { }
Expr::Expr(double value, string name): shared_ptr<Expression>(new Variable(value, name)) { }

int main() {
    Expr x = make_expr<Variable>(3, "x");
    Expr y = make_expr<Variable>(3, "y");

    auto expr = pow(log(x), cos(y));

    cout << expr->value() << endl;
    cout << expr << endl;
    cout << expr->derivative(y)->value() << endl;
    cout << expr->derivative(y) << endl;
    cout << expr->derivative(y)->simplify() << endl;
    cout << sqrt(x) << endl;

    return 0;
}
