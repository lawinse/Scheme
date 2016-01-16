#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <iterator>
#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <ctime>
#include <string>
#include <algorithm>
#include <map>
#include <set>
#include <vector>
#include <queue>
#include <bitset>
#include <cctype>
#include <cassert>
#include <complex>



#include "Longint.h"
#include "Exception.h"

#define SCAST_N(x) static_cast<Num*>(x)
#define SCAST_R(x) static_cast<RlNum*>(x)
#define SCAST_B(x) static_cast<Bool*>(x)
#define SCAST_S(x) static_cast<String*>(x)
#define SCAST_CH(x) static_cast<Char*>(x)
#define SCAST_CMPLX(x) static_cast<Cmplx*>(x)
#define SCAST_FLOAT(x) static_cast<Float*>(x)
#define SCAST_INT(x) static_cast<Int*>(x)
#define SCAST_RATIONAL(x) static_cast<Rational*>(x)

using namespace std;

const bool sanguan = 1;

//==全局map&set========================
string _Procedure []={"+","-","*","/",
                    "=","<=",">=","<",">",
                    "zero?","positive?","negative?","odd?","even?",
                    "sin","cos","tan","log","asin",
                    "acos","atan","sqrt","exp","max",
                    "min","abs","magnitude","make-rectangular","make-polar",
                    "real-part","imag-part","angle","equal?","number?",
                    "integer?","real?","rational?","exact?","inexact?",
                    "complex?","quotient","remainder","modulo","gcd",
                    "lcm","numerator","denominator","floor","ceiling",
                    "round","truncate","expt","newline","exact->inexact",
                    "inexact->exact","number->string","string->number","boolean?","not",
                    "string?","make-string","string-length","string-ref","string=?",
                    "string-ci=?","string<?","string>?","string<=?","string>=?",
                    "string-ci<?","string-ci>?","string-ci<=?","string-ci>=?","substring",
                    "string-append","string-copy","display","char?","char=?",
                    "char-ci=?","char<?","char>?","char<=?","char>=?",
                    "char-ci<?","char-ci>?","char-ci<=?","char-ci>=?","char-alphabetic?",
                    "char-numeric?","char-whitespace?","char-upper-case?","char-lower-case?","char->integer",
                    "integer->char","char-upcase","char-downcase","string","procedure?"};

const std::map<char, char>::value_type init_value[] = {
                    std::map<char, char>::value_type( 'a', '\a'),
                    std::map<char, char>::value_type( 'b', '\b'),
                    std::map<char, char>::value_type( 'f', '\f'),
                    std::map<char, char>::value_type( 'n', '\n'),
                    std::map<char, char>::value_type( 'r', '\r'),
                    std::map<char, char>::value_type( 't', '\t'),
                    std::map<char, char>::value_type( 'v', '\v'),
                    std::map<char, char>::value_type( '0', '\0'),
                    std::map<char, char>::value_type( '\\', '\\'),
                    std::map<char, char>::value_type( '\'', '\''),
                    std::map<char, char>::value_type( '\"', '\"'),
};

std::set<string> Procedure(_Procedure,_Procedure+sizeof(_Procedure)/sizeof(_Procedure[0]));
std::map<char, char> zy_mp(init_value, init_value+11);



//==辅助函数==============================
//==高精类3个转换函数=======================================
Longint strtoLint(string _str, bool &flg, int _base)
{
    map<char, int>value;
    value['0'] = 0, value['1'] = 1, value['2'] = 2, value['3'] = 3;
    value['4'] = 4, value['5'] = 5, value['6'] = 6, value['7'] = 7;
    value['8'] = 8, value['9'] = 9, value['a'] = 10, value['b'] = 11;
    value['c'] = 12, value['d'] = 13, value['e'] = 14, value['f'] = 15;
    flg = 1;
    bool sign = 0;
    string str = _str;
    if (str[0] == '-') sign =1, str = str.substr(1,str.length()-1);
    Longint ans = 0;
    Longint base = _base;
    Longint tmp = 1;
    switch (_base)
    {
    case 2:

        for (int i=0;i<str.length();++i)
        {
            if (str[i]<'0'||str[i]>'1')
            {
                flg = 0;
                break;
            }
        }
        if (!flg) return Longint(0);
        break;

    case 8:

        for (int i=0;i<str.length();++i)
        {
            if (str[i]<'0'||str[i]>'7')
            {
                flg = 0;
                break;
            }
        }
        if (!flg) return Longint(0);
        break;

    case 16:
        for (int i=0;i<str.length();++i)
        {
            if (str[i]<'0'||str[i]>'9' && str[i]<'a' ||str[i] > 'f')
            {
                flg = 0;
                break;
            }
        }
        if (!flg) return Longint(0);
        break;

    case 10:

        for (int i=0;i<str.length();++i)
        {
            if (str[i]<'0'||str[i]>'9')
            {
                flg = 0;
                break;
            }
        }
        if (!flg) return Longint(0);
        break;
    }
    if(_base != 10)
    {
        reverse(str.begin(),str.end());
        for (int i=0;i<str.length();++i)
        {
            if (str[i] != '0')
            ans = ans + tmp * Longint(value[str[i]]);
            tmp = tmp * base;
        }
        if (sign == 1) ans = Longint(0)-ans;
        return ans;
    }
    else
        return Longint(_str);
}

Longint fromDec (Longint li, int _base)
{
    map<string, char>num2ch;
    num2ch["0"] = '0', num2ch["1"] = '1', num2ch["2"] = '2', num2ch["3"] = '3';
    num2ch["4"] = '4', num2ch["5"] = '5', num2ch["6"] = '6', num2ch["7"] = '7';
    num2ch["8"] = '8', num2ch["9"] = '9', num2ch["10"] = 'a', num2ch["11"] = 'b';
    num2ch["12"] = 'c', num2ch["13"] = 'd', num2ch["14"] = 'e', num2ch["15"] = 'f';
    Longint base = _base;
    bool flg = 1;
    if (li < Longint(0)) li = abs(li), flg = 0;
    string res = "";
    while(li / base != 0)
    {
        res += num2ch[(li % base).s];
        li /= base;
    }
    res += num2ch[li.s];
    reverse (res.begin(),res.end());
    if (!flg) return Longint("-"+res);
    else return Longint (res);

}

Longint str2int(string _str, int base,bool &flg)
{
    Longint tmp = strtoLint (_str,flg,base);
    if (!flg)    return 0;
    flg = true;
    return tmp;
}

char *strdup (const char *s)
{
  size_t len = strlen (s) + 1;
  void *new1 = malloc (len);
  if (new1 == NULL)
    return NULL;
  return (char *) memcpy (new1, s, len);
}

void F2R(double x,Longint &a,Longint &b)
{
    ///==unsure!! why double is wrong?
    double m;
    Longint g,n;
    m=abs(x);n=1;
    //int t = 100;
    while(m-long(m)> 0)  {m=m*10;n=n*10;}
    ///10.0改成10
    g=gcd(Longint(m),n);
    a=Longint(m)/g,b=n/g;
    if (x<0) a=-a;
}

int get_base(string &_str)
{
    int base = 10;
    if (_str.substr(0,2) == "#b") base = 2,_str = _str.substr(2,_str.length()-2);
    else if (_str.substr(0,2) == "#o") base = 8,_str = _str.substr(2,_str.length()-2);
    else if (_str.substr(0,2) == "#x") base = 16,_str = _str.substr(2,_str.length()-2);
    else if (_str.substr(0,2) == "#d") base = 10,_str = _str.substr(2,_str.length()-2);
    if (_str[0] == '+') _str = _str.substr(1,_str.length());
    return base;
}

double str2double(string _str, bool &flg)
{
    char *tptr;
    const char *hptr=_str.c_str();
    double tmp = strtod(hptr, &tptr);
    if (tptr == hptr || tptr != hptr + _str.length())
    {
        flg = 0;
        return 0;
    }
    flg = true;
    return tmp;
}
void low_down(string &s)
{
    transform(s.begin(), s.end(), s.begin(), ::tolower);
    return;
}

void low_down(char &ch)
{
    if(ch <= 'Z' && ch >= 'A') ch += 32;
    return;
}

//===数据类型=============================

class Num
{
public:
    //double x;
    enum{YES = 1, NO = 0} dsply;
    virtual void C2DS(){dsply = YES;}
    enum {UNEXACT=0,EXACT=1}exact;
    enum {PROD = -4, CHAR = -3, STR=-2,BOOL=-1,INT=0,RATIONAL=1,FLOAT=2,COMPLEX=3}tplvl;
    enum {Q=1,CrQ=0,NaR=-1}type;
    virtual ~Num(){};
    virtual Num* add(Num *b) = 0;
    virtual Num* sub(Num *b) = 0;
    virtual Num* mul(Num *b) = 0;
    virtual Num* div(Num *b) = 0;
    virtual Num* convert (Num *b) = 0;
    virtual void print () = 0;
    virtual void display() = 0;
    virtual void exact2un(){exact = UNEXACT;}
    virtual void exact2ex(){exact = EXACT;}
    virtual void Q2CrQ(){type = CrQ;}
    virtual void CrQ2Q(){type = Q;}
    virtual string toString() = 0;

};

class Prod:public Num
{
public:
    string name;
    Prod(){ dsply = NO;tplvl = PROD;type = NaR ;exact = EXACT; }
    Prod (const string &s){dsply = NO;tplvl = PROD; name = s;type = NaR ;exact = EXACT;}
    virtual ~Prod(){};
    virtual Num* add(Num *b) {}
    virtual Num* sub(Num *b) {}
    virtual Num* mul(Num *b) {}
    virtual Num* div(Num *b) {}
    virtual Num* convert (Num *b) {}
    virtual string toString() {}
    virtual void print(){cout << "#<procedure:" << name << ">";}
    virtual void display() {print();}
    static Prod *from_str(string str)
    {
            set<string>::iterator it;
            if ((it=Procedure.find(str)) != Procedure.end())
                return new Prod(str);
            else return NULL;
    }
};

class Bool:public Num
{
public:
    int x;
    Bool(){ dsply = NO;tplvl = BOOL;type = NaR ;exact = EXACT; }
    Bool(int flg)
    {
        dsply = NO;tplvl = BOOL;type = NaR ;exact = EXACT;
        if (flg == 1) x = 1;
        else x = 0;
    }
    virtual ~Bool(){}
    virtual void print()
    {
        if (x == 1) cout << "#t";
        else cout << "#f";
    }
    virtual void display (){print();}
    virtual Num* add(Num *b) {}
    virtual Num* sub(Num *b) {}
    virtual Num* mul(Num *b) {}
    virtual Num* div(Num *b) {}
    virtual Num* convert (Num *b) {}
    void setF(){x = 0; }
    void setT(){x = 1; }
    virtual string toString(){}
    static Bool *from_str(string str)
    {
        if (str == "#t") return new Bool(1);
        else if (str == "#f") return new Bool(0);
        else return NULL;
    }

};


class String: public Num
{
public:
    string str;
    String(){ dsply = NO;tplvl = STR;type = NaR ;exact = EXACT; }
    String (const string &s){dsply = NO;tplvl = STR; str = s;type = NaR ;exact = EXACT;}
    virtual ~String(){};
    virtual Num* add(Num *b) {}
    virtual Num* sub(Num *b) {}
    virtual Num* mul(Num *b) {}
    virtual Num* div(Num *b) {}
    virtual Num* convert (Num *b) {}
    virtual string toString() {}
    virtual void print(){ cout << "\"" << str << "\"" ;}
    virtual void display(){cout << str ;}

};

class Char :public Num
{
public:
    char chr;
    Char() {dsply = NO;tplvl = CHAR; type = NaR; exact = EXACT;}
    Char(const char &_ch) {dsply = NO;tplvl = CHAR; type = NaR; exact = EXACT;chr = _ch;}
    Char(const int &_a) {dsply = NO;tplvl = CHAR; type = NaR; exact = EXACT;chr = _a;}
    virtual ~Char(){};
    virtual Num* add(Num *b) {}
    virtual Num* sub(Num *b) {}
    virtual Num* mul(Num *b) {}
    virtual Num* div(Num *b) {}
    virtual Num* convert (Num *b) {}
    virtual string toString() {}
    virtual void print(){ cout << "#" << "\\" <<  chr ;}
    virtual void display(){cout << chr;}
    static Num *from_str(string str)
    {
        if (str.substr(0,2) != "#\\") return NULL;
        else if (str.length() != 3)
        {
            string tmp = str.substr(2,str.length()-2);
            if (tmp == "newline") return new Char(10);
            else if (tmp == "nul") return new Char(0);
            else if (tmp == "vtab") return new Char(11);
            else if (tmp == "space") return new Char(32);
            else if (tmp == "tab") return new Char(9);
            else if (tmp == "page") return new Char(12);
            else if (tmp == "linefeed") return new Char(10);
            else if (tmp == "return") return new Char(13);
            else if (tmp == "backspace") return new Char(8);
            else throw NOChar();

        }
        else
        {
            char tmp_ch = str[2];
            return new Char (tmp_ch);
        }
    }
};

class RlNum:public Num
{
public:
    virtual ~RlNum(){};
    virtual Num* add(Num *b) {}
    virtual Num* sub(Num *b) {}
    virtual Num* mul(Num *b) {}
    virtual Num* div(Num *b) {}
    virtual Num* convert (Num *b) {}
    virtual void print () {}
    virtual string toString() = 0;
    //virtual void exact2un(){exact = UNEXACT;}
    //virtual void exact2ex(){exact = EXACT;}
    // virtual void Q2CrQ(){type = CrQ;}
};

//=======复数！！！===============

double f_from_tplvl(Num *a);
double f_from_tplvl(RlNum *a);

class Cmplx:public Num
{
public:
    RlNum *real;
    RlNum *imag;
    double x;
    Cmplx(){ dsply = NO;tplvl = COMPLEX; exact = EXACT;type = NaR;}
    ///==unsure!!
    Cmplx(RlNum* r, RlNum* i){dsply = NO;real = r, imag = i; tplvl = COMPLEX; exact = EXACT;type = NaR;}
    ///!!!
    //virtual ~Cmplx(){delete real;delete imag;};
    friend Num* create_Cmplx(RlNum *r, RlNum *i);

    virtual Num* add(Num *b);

    virtual Num* sub(Num *b) ;
    virtual Num* mul(Num *b) ;
    virtual Num* div(Num *b) ;
    virtual Num* convert (Num *b);
    virtual void print ()
    {
        real->print();
        if(f_from_tplvl(imag) >= 0)cout << "+" ;
        imag->print();
        cout << "i" ;
    };
    virtual void display(){print();}
    virtual string toString();
//    virtual void exact2un(){exact = UNEXACT;}
//    virtual void exact2ex(){exact = EXACT;}
//    virtual void Q2CrQ(){type = CrQ;}
    static Num *from_str(string str);



};


class Float: public RlNum
{
public:
    double x;
    Float(){ dsply = NO;tplvl = FLOAT; exact = EXACT;type = Q;}
    Float(double tmpx,int a = 1):x(tmpx){dsply = NO; tplvl = FLOAT; exact = EXACT;if (a == 1) type = Q;else type = CrQ;}
    virtual ~Float(){};
    virtual Num* add(Num *b) ;
    virtual Num* sub(Num *b) ;
    virtual Num* mul(Num *b) ;
    virtual Num* div(Num *b) ;
    virtual Num* convert(Num *b);
    virtual void print(){ cout << setprecision(20) << x; }
    virtual void display(){print();}
    friend Num* create_Float(double x, int c);
    virtual string toString()
    {
        stringstream tmpstr;
        tmpstr << setprecision(20) <<x;
        string str (tmpstr.str());
        return str;
    }
    static Float *from_str(string str)
    {
        bool flg;
        double tmp = str2double(str, flg);
        if (!flg) return NULL;
        return new Float(tmp);
    }



};
class Rational: public RlNum
{
public:
    Longint fenzi;
    Longint fenmu;
    double x;
    void reduction()
    {

        Longint n = gcd (fenzi>Longint(0)?fenzi:-fenzi,fenmu>Longint(0)?fenmu:-fenmu);

        fenzi /= n; fenmu /= n;
        if (fenmu < Longint(0)) fenzi = -fenzi, fenmu = -fenmu;

    }
    Rational(){dsply = NO;tplvl = RATIONAL;exact = EXACT;type = Q;}
    Rational(Longint a, Longint b=1):fenzi(a), fenmu(b){dsply = NO;x = Longint2double(fenzi)/fenmu; tplvl = RATIONAL;reduction();exact = EXACT;type = Q;}
    virtual ~Rational() {}
    /// unsure !---------
    virtual Num* add(Num *b);
    virtual Num* sub(Num *b);
    virtual Num* mul(Num *b);
    virtual Num* div(Num *b);
    ///-------------------
    virtual void print(){if (fenmu==1)cout << fenzi;else cout << fenzi << "/" << fenmu; }
    virtual void display () {print();}
    virtual Num* convert(Num *b);
    friend Num* create_Rational(Longint a, Longint b);
    virtual string toString()
    {
        stringstream tmpstr;
        if (fenmu==1)tmpstr << fenzi;else tmpstr << fenzi << "/" << fenmu;
        string str (tmpstr.str());
        return str;
    }
    static Num* from_str(string str);
};

class Int: public RlNum
{
public:
    Longint x;
    Int(){ dsply = NO;tplvl = INT;exact = EXACT; type = Q;}
    Int(Longint tmpx):x(tmpx){dsply = NO; tplvl = INT; exact = EXACT;type = Q;}
    virtual ~Int(){}
    virtual Num* add(Num *b) { return new Int(x + SCAST_INT(b)->x); }
    virtual Num* sub(Num *b) { return new Int(x - SCAST_INT(b)->x); }
    virtual Num* mul(Num *b) { return new Int(x * SCAST_INT(b)->x); }
    virtual Num* div(Num *b) ;
    virtual Num* convert(Num *b);
    virtual void print(){ cout << x; }
    virtual void display(){print();}
    virtual string toString()
    {
        stringstream tmpstr;
        tmpstr << x;
        string str (tmpstr.str());
        return str;
    }
    static Int *from_str(string str)
    {
        bool flg;
        int base =  get_base(str);
        Longint tmp = str2int(str,base,flg);
        if (!flg) return NULL;
        return new Int(tmp);
    }
};


//==两个取值函数===============
double f_from_tplvl(Num *a)
{
    double tmp;
    if (a->tplvl==0)
            tmp = Longint2double(SCAST_INT(a)->x);
        else if (a->tplvl==1)
            tmp = SCAST_RATIONAL(a)->x;
        else if (a->tplvl==2)
            tmp = SCAST_FLOAT(a)->x;
        else if(a->tplvl == -1)
            tmp = SCAST_B(a)->x;
    return tmp;
}

double f_from_tplvl(RlNum *a)
{
    double tmp;
    if (a->tplvl==0)
            tmp = Longint2double(SCAST_INT(a)->x);
        else if (a->tplvl==1)
            tmp = SCAST_RATIONAL(a)->x;
        else if (a->tplvl==2)
            tmp = SCAST_FLOAT(a)->x;
    return tmp;
}

//==lans====================
Num *lans = new Int(0);

//==写在外面的函数==========
Num *Int :: convert(Num *b)
    {
        if (b->tplvl > tplvl ) throw NOlow2high();
        Int *res = new Int();
        bool _exact = b->exact;
        int _type = b->type;
        switch (b->tplvl)
        {
            //case BOOL: res->x = SCAST_B(b)->x; break;
            case INT: res->x = SCAST_INT(b)->x; break;
            default : throw NOtype();
        }
        if (!_exact) res->exact2un();
        if (_type == 0) res->Q2CrQ();
        return res;
    }

Num *Rational::convert(Num *b)
 {
        if (b->tplvl > tplvl ) throw NOlow2high();
        Rational *res = new Rational();
        bool _exact = b->exact;
        int _type = b->type;
        switch (b->tplvl)
        {
            //case BOOL: res->x = SCAST_B(b)->x; break;
            case INT: res->fenzi = SCAST_INT(b)->x, res->fenmu = 1; break;
            case RATIONAL: res->fenzi = SCAST_RATIONAL(b)->fenzi, res->fenmu = SCAST_RATIONAL(b)->fenmu; break;
            default :throw NOtype();
        }
        if (!_exact) res->exact2un();
        if (_type == 0) res->Q2CrQ();
        return res;
    }

Num* create_Rational(Longint a, Longint b,int c=1)
{
    Num* res;
    if (b == Longint(0)) throw DtrNOzero();
    if (a==Longint(0)) res = new Int(0);
    else if (a/b*b == a) res = new Int(a/b);
    else res = new Rational(a,b);
    if (c == Longint(0)) res->exact2un();
    return res;
}

Num* create_Float(double x,int c = 1)
{
    Num *res;
    Longint fz,fm;
    if (fabs(x)-0 == 0) res = new Int(0);
    else if(abs(x - (Longint)(int(x)) ) == 0) res = new Int((Longint)(int(x)));
    //else if (c=1) F2R(x,fz,fm), res = create_Rational(fz,fm)
    else res = new Float(x,c);
    if (c == 1) res->exact2un();
    return res;
}


Num* create_Cmplx(RlNum *r, RlNum *i,int c=1)
{
    Num *res;
    if (r->exact == 0 || i->exact == 0) c=0;
    if (f_from_tplvl(i) == 0.0) res = r;
    else res = new Cmplx(r,i);
    if (c == 0)res->exact2un();
    return res;
}

Num* Float::add(Num *b) { return create_Float(x + SCAST_FLOAT(b)->x); }
Num* Float::sub(Num *b) { return create_Float(x - SCAST_FLOAT(b)->x); }
Num* Float::mul(Num *b) { return create_Float(x * SCAST_FLOAT(b)->x); }
Num* Float::div(Num *b) { return create_Float(x / SCAST_FLOAT(b)->x); }

Num* Rational::add(Num *b)
    {
        return create_Rational(fenzi * SCAST_RATIONAL(b)->fenmu + fenmu * SCAST_RATIONAL(b)->fenzi, fenmu * SCAST_RATIONAL(b)->fenmu);
    }
Num* Rational::sub(Num *b)
    {
        return create_Rational(fenzi * SCAST_RATIONAL(b)->fenmu - fenmu * SCAST_RATIONAL(b)->fenzi, fenmu * SCAST_RATIONAL(b)->fenmu);
    }
Num* Rational::mul(Num *b)
    {
        return create_Rational(fenzi * SCAST_RATIONAL(b)->fenzi, fenmu * SCAST_RATIONAL(b)->fenmu);
    }
Num* Rational::div(Num *b)
    {
        return create_Rational(fenzi * SCAST_RATIONAL(b)->fenmu, fenmu * SCAST_RATIONAL(b)->fenzi);
    }


Num* Int::div(Num *b){ return create_Rational((x) , SCAST_INT(b)->x); }

Num* Rational::from_str(string str)
    {
        bool flg;
        int loc;
        Longint fz,fm;
        if ((loc = str.find("."))!=string::npos)
        {
            int times = (int)str.length() - loc -1;
            string tmp = str.substr(0,loc)+str.substr(loc+1,times);
            int base = get_base(tmp);
            fz = str2int(tmp,base,flg);
            if (!flg) return NULL;
            fm = Longint(1);
            while(times--) fm *= Longint(10);
            bool flg = 0;
            fm = strtoLint(fm.s,flg,base);
            if (!flg) return NULL;
            return create_Rational(fz,fm,0);
        }

        else if ((loc = str.find("/"))!=string::npos)
        {

            string str_fz = str.substr(0,loc);
            string str_fm = str.substr(loc+1,str.length()-1-loc);
            Longint fz,fm;
            bool f1,f2;
            int base1 = get_base(str_fz);
            int base2 = get_base(str_fm);
            fz=str2int(str_fz,base1,f1);
            fm=str2int(str_fm,base2,f2);

            if (f1&&f2)
            {
                if (fm == Longint(0)) throw DtrNOzero();
                return create_Rational(fz,fm);
            }
            else return NULL;

        }
        else return NULL;
    }


Num* Cmplx::from_str(string str)
{
    bool flag;
    int loc1, loc2,loc_mid;
    string _real, _imag;
    Num *real_, *imag_;
    Num *len_, *angle_;
    if (((loc_mid) = str.find("@") )!= string::npos)
    {
        string _len = str.substr(0,loc_mid);
        string _angle = str.substr(loc_mid+1, str.length()-loc_mid-1);
        if (_angle[0] == '#') return NULL;
        if (_len[0] == '#') _angle = _len.substr(0,2)+_angle;
        len_ = Int::from_str(_len);
        if (!len_) len_ = Rational::from_str(_len);
        if (!len_) len_ = Float::from_str(_len);
        if (!len_) return NULL;

        angle_ = Int::from_str(_angle);
        if (!angle_) angle_ = Rational::from_str(_angle);
        if (!angle_) angle_ = Float::from_str(_angle);
        if (!angle_) return NULL;

        double x = f_from_tplvl(len_) * cos(f_from_tplvl(angle_));
        double y = f_from_tplvl(len_) * sin(f_from_tplvl(angle_));


        real_ = create_Float(x,0);
        imag_ = create_Float(y,0);

    }
    else {
    if (str.find("+") == string::npos && str.find("-") == string::npos || (int)str.find("i") != str.length()-1 )
        return NULL;

    if ((int)str.find("i") != str.length()-1) return NULL;
    loc2 = str.find("i");
    if (str == "+i") _real = "", _imag = "";
    else if (str == "-i") _real = "", _imag ="-";
    else
    {
        int i;
        for (i=str.length()-1;i>=0;--i)
        {
            if (str[i] == '+' || str[i] == '-') {loc1 = i;break;}
        }
        if (i<0) return NULL;
        if (loc1 > 0 && str[loc1 - 1] =='e' )
        {
            for ( i=loc1-1;i>=0;--i)
            {
                if (str[i] == '+' || str[i] == '-') {loc1 = i;break;}
            }
            if (i<0) return NULL;
        }
        _real = str.substr(0,loc1);
        _imag = str.substr(loc1,loc2-loc1);
    }

    if (_imag[0] == '#') return NULL;
    if (_real == "") _real = "0";
    if (_imag == "") _imag = "1";
    if (_imag == "+") _imag ="1";
    if (_imag == "-")_imag = "-1";
    if (_real[0] == '#') _imag = _real.substr(0,2)+_imag;
    real_ = Int::from_str(_real);
    if (!real_) real_ = Rational::from_str(_real);
    if (!real_) real_ = Float::from_str(_real);
    if (!real_) return NULL;

    imag_ = Int::from_str(_imag);

    if (!imag_) imag_ = Rational::from_str(_imag);
    if (!imag_) imag_ = Float::from_str(_imag);
    if (!imag_) return NULL;


    }


    return create_Cmplx(SCAST_R(real_),SCAST_R(imag_));

}


Num *Float::convert(Num *b)
{
        if (b->tplvl > tplvl) throw NOlow2high();
        Float *res = new Float();
        bool _exact = b->exact;
        int _type = b->type;
        //cout << "b_type" << b->type <<endl;
        switch (b->tplvl)
        {
            //case BOOL: res->x = SCAST_B(b)->x; break;
            case INT: res->x = SCAST_INT(b)->x+0.0; break;
            /// unsure !-----
            case RATIONAL: res->x = SCAST_RATIONAL(b)->x; break;
            ///--------------
            case FLOAT: res->x = SCAST_FLOAT(b)->x; break;
            default : throw NOtype();
        }
        if (!_exact) res->exact2un();
        if (_type == 0) res->Q2CrQ();
        return res;

}

Num *Cmplx::convert(Num *b)
{
        if (b->tplvl > tplvl) throw NOlow2high();
        Cmplx *res = new Cmplx();
        bool _exact = b->exact;
        int _type = b->type;
        //cout << "b_type" << b->type <<endl;
        switch (b->tplvl)
        {
            //case BOOL: res->real = new Int(SCAST_B(b)->x); break;
            case INT: res->real = new Int(SCAST_INT(b)->x);break;
            /// unsure !-----
            case RATIONAL: res->real = new Rational(SCAST_RATIONAL(b)->fenzi,SCAST_RATIONAL(b)->fenmu); break;
            ///--------------
            case FLOAT: res->real = new Float(SCAST_FLOAT(b)->x); break;
            case COMPLEX: res->real = SCAST_CMPLX(b)->real; res->imag = SCAST_CMPLX(b)->imag;break;
            default : throw NOtype();
        }
        if (b->tplvl<3) res->imag = new Int(0);
        if (!_exact) res->real->exact2un();
        if (_type == 0) res->real->Q2CrQ() ;

        return res;
}


string Cmplx::toString()
{
    string str = "";
    str = real->toString();
    if (f_from_tplvl(imag) == 0.0) return str;
    else if (f_from_tplvl(imag) > 0.0) str += "+";
    str += imag->toString();
    str += 'i';
    return str;
}


//==数据链=======================
struct Numlks
{
    Num *value;
    Numlks *next;
    Numlks(Num *_value, Numlks *_next):value(_value), next(_next){}
    bool check_len(int len)
    {
        int l = 0;
        for (Numlks *p = this; p; p = p->next) l++;
        return l == len;
    }
    int get_len ()
    {
        int l = 0;
        for (Numlks *p = this; p; p = p->next) l++;
        return l;
    }
    bool is_exact()
    {
        bool flg = 1;
        for (Numlks *p = this; p; p = p->next)
            if (p->value->exact == Num::UNEXACT) {flg = 0; break;}
        return flg;
    }

//    bool is_RQ()
//    {
//        bool flg = 1;
//        for (Numlks *p = this; p; p = p->next)
//            if (p->value->tplvl==3 && SCAST_CMPLX(p->value)->real->type != Num::Q) {flg = 0; break;}
//        return flg;
//    }

    bool is_IQ()
    {
        bool flg = 1;
        for (Numlks *p = this; p; p = p->next)
            if (p->value->tplvl==3 && SCAST_CMPLX(p->value)->imag->type != Num::Q) {flg = 0; break;}
        return flg;
    }


    bool is_Q()
    {
        bool flg = 1;
        for (Numlks *p = this; p; p = p->next)
            if (p->value->tplvl<3 && p->value->type != Num::Q||p->value->tplvl==3 && SCAST_CMPLX(p->value)->real->type != Num::Q) {flg = 0; break;}
        return flg;
    }
    bool is_all_postive()
    {
        bool flg = 1;
        for (Numlks *p = this; p; p = p->next)
        if (f_from_tplvl(p->value) <= 0){flg = 0; break; }
        return flg;
    }
    bool is_all_less_type(int a)
    {
        bool flg = 1;
        for (Numlks *p = this; p; p = p->next)
        if (p->value->tplvl > a){flg = 0; break; }
        return flg;
    }
    bool is_all_higher_type(int a)
    {
        bool flg = 1;
        for (Numlks *p = this; p; p = p->next)
        if (p->value->tplvl < a){flg = 0; break; }
        return flg;
    }
    bool is_all_type(int a)
    {
        bool flg = 1;
        for (Numlks *p = this; p; p = p->next)
        if (p->value->tplvl != a){flg = 0; break; }
        return flg;
    }
};

void del_Numlks(Numlks *numlk)
{
    for (Numlks *tmp; numlk; )
    {
        tmp = numlk->next;
        delete numlk;
        numlk = tmp;
    }

}

//==运算符类型=====================
class Opt
{
public:
    virtual Num* calc (Numlks *numlk) = 0;
};

class Add : public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
         bool f = numlk->is_exact();
        bool fq = numlk->is_Q();
        bool fiq = numlk->is_IQ();
        Num *res = new Int(0), *old;
        int len = numlk->get_len();
        //cout << len <<endl;
        if (len > 0)
        {
        Num *opr1 = numlk->value, *tmp1;
        old = res;
        if (res->tplvl > opr1->tplvl)
        {
            tmp1 = res->convert(opr1);
            res = res->add(tmp1);
            //if (!fq) res->Q2CrQ();
        }
        else
        {
            tmp1 = opr1->convert(res);
            res = tmp1->add(opr1);

        }
        if (!fq)
        {
            res->Q2CrQ();
            if (res->tplvl == 3) SCAST_CMPLX(res)->real->Q2CrQ();
        }
        if (!fiq) if (res->tplvl == 3) SCAST_CMPLX(res)->imag->Q2CrQ();
        delete old;
        delete tmp1;
        }
        if (len > 1){
        numlk = numlk->next;

        for (; numlk; numlk = numlk->next)
        {
            Num *opr = numlk->value, *tmp;
            old = res;
            if (res->tplvl > opr->tplvl)
            {
                tmp = res->convert(opr);
                res = res->add(tmp);
                //if (!f) res->exact2un();
            }
            else
            {
                tmp = opr->convert(res);
                res = tmp->add(opr);
                //if (!f) res->exact2un();
            }

            delete old;
            delete tmp;
        }
        }
        if (!f) res->exact2un();
        if (!fq)
        {
            res->Q2CrQ();
            if (res->tplvl == 3) SCAST_CMPLX(res)->real->Q2CrQ();
        }
        if(!fiq) if( res->tplvl == 3) SCAST_CMPLX(res)->imag->Q2CrQ();
//        if (res->type == Num::Q && res->tplvl ==2)
//            {
//                Longint fenmu,fenzi;
//                F2R(f_from_tplvl(res),fenzi,fenmu);
//                old = res;
//                res = create_Rational(fenzi,fenmu);
//                delete old;
//            }
        res = SCAST_N(res);
        return res;
    }
};
class Sub : public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
       bool f = numlk->is_exact();
        //bool frq = numlk->is_RQ();
        bool fiq = numlk->is_IQ();
        bool fq = numlk->is_Q();

        Num *res = new Int(0), *old;
      // if (!fq) res->Q2CrQ();
        if (numlk->get_len() == 0) throw Ag2less();
        if (numlk->get_len() == 1)
        {

            Num *opr1 = numlk->value, *tmp1;
            old = res;
            if (res->tplvl > opr1->tplvl)
            {

                tmp1 = res->convert(opr1);
                res = res->sub(tmp1);

            }
            else
            {

                tmp1 = opr1->convert(res);
                res = tmp1->sub(opr1);

            }
            delete old;
            delete tmp1;
            if (!f) res->exact2un();
            if (!fq) res->Q2CrQ();
            return res;

        }
        ///unsure!!!
        Num *opr1 = numlk->value, *tmp1;
        old = res;
        if (res->tplvl > opr1->tplvl)
        {
            tmp1 = res->convert(opr1);
            res = res->add(tmp1);
        }
        else
        {
            tmp1 = opr1->convert(res);
            res = tmp1->add(opr1);

        }
        if (!fq)
        {
            res->Q2CrQ();
            if (res->tplvl == 3) SCAST_CMPLX(res)->real->Q2CrQ();
        }
        if (!fiq) if (res->tplvl == 3) SCAST_CMPLX(res)->imag->Q2CrQ();
        delete old;
        delete tmp1;

        numlk = numlk->next;
        ///
        for (; numlk; numlk = numlk->next)
        {
            Num *opr = numlk->value, *tmp;
            old = res;

            if (res->tplvl > opr->tplvl)
            {
                tmp = res->convert(opr);
                res = res->sub(tmp);

            }
            else
            {
                tmp = opr->convert(res);
                res = tmp->sub(opr);
            }
            delete old;
            delete tmp;
        }
        if (!f) res->exact2un();
        if (!fq)
        {
            res->Q2CrQ();
            if (res->tplvl == 3) SCAST_CMPLX(res)->real->Q2CrQ();
        }
        if(!fiq) if( res->tplvl == 3) SCAST_CMPLX(res)->imag->Q2CrQ();
        res = SCAST_N(res);

        return res;
    }
};

class Mul : public Opt
{

        ///==!!flaggg!
         Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        bool f = numlk->is_exact();
        bool fq = numlk->is_Q();
        bool fiq = numlk->is_IQ();
        Num *res = new Int(1), *old;
        int len = numlk->get_len();
        if (len > 0){
        Num *opr1 = numlk->value, *tmp1;
        old = res;
        if (res->tplvl > opr1->tplvl)
        {
            tmp1 = res->convert(opr1);
            res = res->mul(tmp1);
        }
        else
        {
            tmp1 = opr1->convert(res);
            res = tmp1->mul(opr1);

        }
        if (!fq)
        {
            res->Q2CrQ();
            if (res->tplvl == 3) SCAST_CMPLX(res)->real->Q2CrQ();
        }
        if (!fiq) if (res->tplvl == 3) SCAST_CMPLX(res)->imag->Q2CrQ();
        delete old;
        delete tmp1;
    }
        if (len > 1){
        numlk = numlk->next;

        for (; numlk; numlk = numlk->next)
        {
            Num *opr = numlk->value, *tmp;
            old = res;
            if (res->tplvl > opr->tplvl)
            {
                tmp = res->convert(opr);
                res = res->mul(tmp);
            }
            else
            {
                tmp = opr->convert(res);
                res = tmp->mul(opr);
            }

            delete old;
            delete tmp;
        }
        }

        if (!f) res->exact2un();
        if (!fq)
        {
            res->Q2CrQ();
            if (res->tplvl == 3) SCAST_CMPLX(res)->real->Q2CrQ();
        }
        if(!fiq) if( res->tplvl == 3) SCAST_CMPLX(res)->imag->Q2CrQ();
        res = SCAST_N(res);

        return res;
    }
};

class Div : public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        bool f = numlk->is_exact();
        //bool frq = numlk->is_RQ();
        bool fiq = numlk->is_IQ();
        bool fq = numlk->is_Q();

        Num *res = new Int(1), *old;
      if (numlk->get_len() == 0) throw Ag2less();
        if (numlk->get_len() == 1)
        {
            double diff = f_from_tplvl(numlk->value);
             if (abs(diff) == 0 ) throw NOdivzero();

            Num *opr1 = numlk->value, *tmp1;
            old = res;
            if (res->tplvl > opr1->tplvl)
            {

                tmp1 = res->convert(opr1);
                res = res->div(tmp1);
            }
            else
            {

                tmp1 = opr1->convert(res);
                res = tmp1->div(opr1);
            }
            delete old;
            delete tmp1;
            if (!f) res->exact2un();
            if (!fq) res->Q2CrQ();


            return res;

        }
        ///unsure!!!
        Num *opr1 = numlk->value, *tmp1;
        old = res;
        if (res->tplvl > opr1->tplvl)
        {
            tmp1 = res->convert(opr1);
            res = res->mul(tmp1);
        }
        else
        {
            tmp1 = opr1->convert(res);
            res = tmp1->mul(opr1);

        }
        if (!fq)
        {
            res->Q2CrQ();
            if (res->tplvl == 3) SCAST_CMPLX(res)->real->Q2CrQ();
        }
        if (!fiq) if (res->tplvl == 3) SCAST_CMPLX(res)->imag->Q2CrQ();
        delete old;
        delete tmp1;

        numlk = numlk->next;
        ///
        for (; numlk; numlk = numlk->next)
        {
            Num *opr = numlk->value, *tmp;
            double diff = f_from_tplvl(opr);
            if (abs(diff)== 0 ) throw NOdivzero();
            old = res;

            if (res->tplvl > opr->tplvl)
            {
                tmp = res->convert(opr);
                res = res->div(tmp);
            }
            else
            {
                tmp = opr->convert(res);
                res = tmp->div(opr);
            }
            delete old;
            delete tmp;
        }
        if (!f) res->exact2un();
        if (!fq)
        {
            res->Q2CrQ();
            if (res->tplvl == 3) SCAST_CMPLX(res)->real->Q2CrQ();
        }
        if(!fiq) if( res->tplvl == 3) SCAST_CMPLX(res)->imag->Q2CrQ();

//        if (res->type == Num::Q && res->tplvl ==2)
//            {
//                Longint fenmu,fenzi;
//                F2R(f_from_tplvl(res),fenzi,fenmu);
//                old = res;
//                res = create_Rational(fenzi,fenmu);
//                delete old;
//            }
        res = SCAST_N(res);
        return res;
    }
};


//==复数加减乘除！！
Num* Cmplx :: add(Num *b)
{

    Num*res_r,*res_i;
    Opt *opt_r = new Add();
    Opt *opt_i = new Add();
    Numlks *numlk = new Numlks(this->real,NULL),*endd = numlk;
    endd ->next = new Numlks(SCAST_CMPLX(b)->real,NULL);
    endd = endd->next;
    res_r = opt_r->calc(numlk);
    Numlks *numlk1 = new Numlks(this->imag,NULL),*endd1 = numlk1;
    endd1 ->next = new Numlks(SCAST_CMPLX(b)->imag,NULL);
    endd1 = endd1->next;
    res_i = opt_i->calc(numlk1);
    delete opt_r;
    delete opt_i;
    del_Numlks(numlk);
    del_Numlks(numlk1);
    return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));
}
Num* Cmplx :: sub(Num *b)
{
    Num*res_r,*res_i;
    Opt *opt_r = new Sub();
    Opt *opt_i = new Sub();
    Numlks *numlk = new Numlks(this->real,NULL),*endd = numlk;
    endd ->next = new Numlks(SCAST_CMPLX(b)->real,NULL);
    endd = endd->next;
    res_r = opt_r->calc(numlk);

    Numlks *numlk1 = new Numlks(this->imag,NULL),*endd1 = numlk1;
    endd1 ->next = new Numlks(SCAST_CMPLX(b)->imag,NULL);
    endd1 = endd1->next;
    res_i = opt_i->calc(numlk1);
    delete opt_r;
    delete opt_i;
    del_Numlks(numlk);
    del_Numlks(numlk1);
    return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));
}

Num* Cmplx :: mul(Num *b)
{
    Num*res_r,*res_i;
    Num *tmp_1,*tmp_2,*tmp_3,*tmp_4;
    Opt *optM = new Mul();
    Opt *optS = new Sub();
    Opt *optA = new Add();
    Num* r1_x = this->real;
    Num* r1_y = this->imag;
    Num* r2_x = SCAST_CMPLX(b)->real;
    Num* r2_y = SCAST_CMPLX(b)->imag;
    Numlks *numlk = new Numlks(r1_x,NULL),*endd = numlk;
    endd ->next = new Numlks(r2_x,NULL);
    endd = endd->next;
    tmp_1 = optM->calc(numlk);
    del_Numlks(numlk);
    delete optM;

    optM = new Mul();
    numlk = new Numlks(r1_y,NULL);endd = numlk;
    endd ->next = new Numlks(r2_y,NULL);
    endd = endd->next;
    tmp_2 = optM->calc(numlk);
    del_Numlks(numlk);
    delete optM;

    optM = new Mul();
    numlk = new Numlks(r1_x,NULL);endd = numlk;
    endd ->next = new Numlks(r2_y,NULL);
    endd = endd->next;
    tmp_3 = optM->calc(numlk);
    del_Numlks(numlk);
    delete optM;

    optM = new Mul();
    numlk = new Numlks(r1_y,NULL);endd = numlk;
    endd ->next = new Numlks(r2_x,NULL);
    endd = endd->next;
    tmp_4 = optM->calc(numlk);
    del_Numlks(numlk);
   delete optM;

    numlk = new Numlks(tmp_1,NULL);endd = numlk;
    endd ->next = new Numlks(tmp_2,NULL);
    endd = endd->next;
    res_r = optS->calc(numlk);
    del_Numlks(numlk);

    numlk = new Numlks(tmp_3,NULL);endd = numlk;
    endd ->next = new Numlks(tmp_4,NULL);
    endd = endd->next;
    res_i = optA->calc(numlk);
    del_Numlks(numlk);

    delete optA;
    delete optS;
    delete tmp_1;
    delete tmp_2;
    delete tmp_3;
    delete tmp_4;
    return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));


}

Num* Cmplx :: div(Num *b)
{
    Num*res_rfz,*res_ifz,*res_rfm,*res_r,*res_i;
    Num *tmp_1,*tmp_2,*tmp_3,*tmp_4;
    Opt *optM = new Mul();
    Opt *optS = new Sub();
    Opt *optA = new Add();
    Opt *optD = new Div();
    Num* r1_x = this->real;
    Num* r1_y = this->imag;
    Num* r2_x = SCAST_CMPLX(b)->real;
    Num* r2_y = SCAST_CMPLX(b)->imag;

    Numlks *numlk = new Numlks(r1_x,NULL),*endd = numlk;
    endd ->next = new Numlks(r2_x,NULL);
    endd = endd->next;
    tmp_1 = optM->calc(numlk);
    del_Numlks(numlk);
    delete optM;

    optM = new Mul();
    numlk = new Numlks(r1_y,NULL);endd = numlk;
    endd ->next = new Numlks(r2_y,NULL);
    endd = endd->next;
    tmp_2 = optM->calc(numlk);
    del_Numlks(numlk);
   delete optM;

    optM = new Mul();
    numlk = new Numlks(r1_x,NULL);endd = numlk;
    endd ->next = new Numlks(r2_y,NULL);
    endd = endd->next;
    tmp_3 = optM->calc(numlk);
    del_Numlks(numlk);
    delete optM;

    optM = new Mul();
    numlk = new Numlks(r1_y,NULL);endd = numlk;
    endd ->next = new Numlks(r2_x,NULL);
    endd = endd->next;
    tmp_4 = optM->calc(numlk);
    del_Numlks(numlk);
    delete optM;

    numlk = new Numlks(tmp_1,NULL);endd = numlk;
    endd ->next = new Numlks(tmp_2,NULL);
    endd = endd->next;
    res_rfz = optA->calc(numlk);
    del_Numlks(numlk);
    delete optA;

    numlk = new Numlks(tmp_4,NULL);endd = numlk;
    endd ->next = new Numlks(tmp_3,NULL);
    endd = endd->next;
    res_ifz = optS->calc(numlk);
    del_Numlks(numlk);
    delete optS;

    optM = new Mul();
    optA = new Add();
    delete tmp_1;
    delete tmp_2;
    delete tmp_3;
    delete tmp_4;

    numlk = new Numlks(r2_x,NULL);endd = numlk;
    endd ->next = new Numlks(r2_x,NULL);
    endd = endd->next;
    tmp_3 = optM->calc(numlk);
    del_Numlks(numlk);
    delete optM;


    optM = new Mul();
    numlk = new Numlks(r2_y,NULL);endd = numlk;
    endd ->next = new Numlks(r2_y,NULL);
    endd = endd->next;
    tmp_4 = optM->calc(numlk);
    del_Numlks(numlk);
    delete optM;

    numlk = new Numlks(tmp_4,NULL);endd = numlk;
    endd ->next = new Numlks(tmp_3,NULL);
    endd = endd->next;
    res_rfm = optA->calc(numlk);
    del_Numlks(numlk);
   delete optA;

    numlk = new Numlks(res_rfz,NULL);endd = numlk;
    endd ->next = new Numlks(res_rfm,NULL);
    endd = endd->next;
    res_r = optD->calc(numlk);
    del_Numlks(numlk);
    delete optD;

    optD = new Div();
    numlk = new Numlks(res_ifz,NULL);endd = numlk;
    endd ->next = new Numlks(res_rfm,NULL);
    endd = endd->next;
    res_i = optD->calc(numlk);
    del_Numlks(numlk);
    delete optD;


    delete tmp_3;
    delete tmp_4;

    return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));


}
//=================================


class Sin : public Opt
{
     Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (numlk->value->tplvl<3 )
        {
            double tmp = f_from_tplvl(numlk->value);
            tmp = sin (tmp);
            Num *res = create_Float(tmp);
            res->exact2un();
            res->Q2CrQ();
            return res;
        }
        else
        {
            Cmplx *opr = SCAST_CMPLX(numlk->value);
            complex<double> num(f_from_tplvl(opr->real),f_from_tplvl(opr->imag));
            complex<double> result = sin(num);
            double _real,_imag;
            _real = result.real();
            _imag = result.imag();
            Num *res_r = create_Float(_real,0),*res_i = create_Float(_imag,0);
            res_r->exact2un(), res_i->exact2un();
            return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));
        }

    }
};

class Cos : public Opt
{
     Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (numlk->value->tplvl<3)
        {
            double tmp = f_from_tplvl(numlk->value);
            tmp = cos (tmp);
            Num *res = create_Float(tmp);
            res->exact2un();
            res->Q2CrQ();
            return res;
        }
        else
        {
            Cmplx *opr = SCAST_CMPLX(numlk->value);
            complex<double> num(f_from_tplvl(opr->real),f_from_tplvl(opr->imag));
            complex<double> result = cos(num);
            double _real,_imag;
            _real = result.real();
            _imag = result.imag();
            Num *res_r = create_Float(_real,0),*res_i = create_Float(_imag,0);
            res_r->exact2un(), res_i->exact2un();
            return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));
        }
    }
};

class Tan : public Opt
{
     Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (numlk->value->tplvl<3)
        {
            double tmp = f_from_tplvl(numlk->value);
            tmp = tan (tmp);
            Num *res = create_Float(tmp);
            res->exact2un();
            res->Q2CrQ();
            return res;
        }
        else
        {
            Cmplx *opr = SCAST_CMPLX(numlk->value);
            complex<double> num(f_from_tplvl(opr->real),f_from_tplvl(opr->imag));
            complex<double> result = tan(num);
            double _real,_imag;
            _real = result.real();
            _imag = result.imag();
            Num *res_r = create_Float(_real,0),*res_i = create_Float(_imag,0);
            res_r->exact2un(), res_i->exact2un();
            return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));
        }
    }
};

class Log : public Opt
{
     Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (numlk->value->tplvl<3)
        {
            double tmp = f_from_tplvl(numlk->value);
            tmp = log (tmp);
            Num *res = create_Float(tmp);
            res->exact2un();
            res->Q2CrQ();
            return res;
        }
        else
        {
            Cmplx *opr = SCAST_CMPLX(numlk->value);
            complex<double> num(f_from_tplvl(opr->real),f_from_tplvl(opr->imag));
            complex<double> result = log(num);
            double _real,_imag;
            _real = result.real();
            _imag = result.imag();
            Num *res_r = create_Float(_real,0),*res_i = create_Float(_imag,0);
            res_r->exact2un(), res_i->exact2un();
            return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));
        }
    }
};

class Asin : public Opt
{
     Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (numlk->value->tplvl<3)
        {
            double tmp = f_from_tplvl(numlk->value);
            if(abs(tmp) > 1)
            {
                complex<double> unit(0,1);
                complex<double> One(1,0);
                complex<double> z(tmp,0);
                complex<double> result = log(z*unit+sqrt(One-pow(z,2)))/unit;
                 double _real,_imag;
                _real = result.real();
                _imag = result.imag();
                Num *res_r = create_Float(_real,0),*res_i = create_Float(_imag,0);
                res_r->exact2un(), res_i->exact2un();
                return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));

            }
            tmp = asin (tmp);
            Num *res = create_Float(tmp);
            res->exact2un();
            res->Q2CrQ();
            return res;
        }
        else
        {
            Cmplx *opr = SCAST_CMPLX(numlk->value);
            complex<double> num(f_from_tplvl(opr->real),f_from_tplvl(opr->imag));
            complex<double> result = asin(num);
            double _real,_imag;
            _real = result.real();
            _imag = result.imag();
            Num *res_r = create_Float(_real,0),*res_i = create_Float(_imag,0);
            res_r->exact2un(), res_i->exact2un();
            return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));
        }
    }
};

class Acos : public Opt
{
     Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (numlk->value->tplvl<3)
        {
            double tmp = f_from_tplvl(numlk->value);
            if(abs(tmp) > 1)
            {
                complex<double> unit(0,1);
                complex<double> One(1,0);
                complex<double> z(tmp,0);
                complex<double> result = log(z+unit*sqrt(One-pow(z,2)))/unit;
                 double _real,_imag;
                _real = result.real();
                _imag = result.imag();
                Num *res_r = create_Float(_real,0),*res_i = create_Float(_imag,0);
                res_r->exact2un(), res_i->exact2un();
                return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));

            }
            tmp = acos (tmp);
            Num *res = create_Float(tmp);
            res->exact2un();
            res->Q2CrQ();
            return res;
        }
        else
        {
            Cmplx *opr = SCAST_CMPLX(numlk->value);
            complex<double> num(f_from_tplvl(opr->real),f_from_tplvl(opr->imag));
            complex<double> result = acos(num);
            double _real,_imag;
            _real = result.real();
            _imag = result.imag();
            Num *res_r = create_Float(_real,0),*res_i = create_Float(_imag,0);
            res_r->exact2un(), res_i->exact2un();
            return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));
        }
    }
};

class Atan : public Opt
{
     Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (numlk->get_len() == 2)
        {
            if (!numlk->is_all_less_type(2)) throw TypeW();
            Num* res;
            Num *x = numlk->value;
            numlk = numlk->next;
            Num *y = numlk->value;
            if (f_from_tplvl(x) == 0 && f_from_tplvl(y) == 0) throw NOallZero();
            if (f_from_tplvl(y) == 0 && f_from_tplvl(x) > 0) res = create_Float(acos(-1)/2);
            else if (f_from_tplvl(y) == 0 && f_from_tplvl(x) < 0) res = create_Float(-acos(-1)/2);
            else if (f_from_tplvl(x) == 0 && f_from_tplvl(y) > 0) res = create_Float(0.0);
            else if (f_from_tplvl(x) == 0 && f_from_tplvl(y) < 0) res = create_Float(acos(-1));
            else
            {
                double tmp = atan(f_from_tplvl(x)/f_from_tplvl(y));
                if (f_from_tplvl(y) < 0)
                {
                    tmp += acos(-1);
                    if (tmp >= acos(-1)) tmp -= 2*acos(-1);
                }
                res = create_Float(tmp);
            }
            return res;

        }
        else if (numlk->value->tplvl<3)
        {
            double tmp = f_from_tplvl(numlk->value);
            tmp = atan (tmp);
            Num *res = create_Float(tmp);
            res->exact2un();
            res->Q2CrQ();
            return res;
        }
        else
        {
            Cmplx *opr = SCAST_CMPLX(numlk->value);
            complex<double> num(f_from_tplvl(opr->real),f_from_tplvl(opr->imag));
            complex<double> result = atan(num);
            double _real,_imag;
            _real = result.real();
            _imag = result.imag();
            Num *res_r = create_Float(_real,0),*res_i = create_Float(_imag,0);
            res_r->exact2un(), res_i->exact2un();
            return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));
        }
    }
};

class Sqrt : public Opt
{
     Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (numlk->value->tplvl<3)
        {
            double tmp = f_from_tplvl(numlk->value);
            if (tmp >= 0)
            {
                tmp = sqrt (tmp);
                Num *res = create_Float(tmp);
                //cout << tmp;
                res->exact2un();
                res->Q2CrQ();
                return res;
            }
            else
            {
                tmp = sqrt(-tmp);
                Num *res_r = create_Float (0,0),*res_i = create_Float(tmp,0);
                res_r->exact2un(), res_i->exact2un();
                return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));
            }

        }
        else
        {
            Cmplx *opr = SCAST_CMPLX(numlk->value);
            complex<double> num(f_from_tplvl(opr->real),f_from_tplvl(opr->imag));
            complex<double> result = sqrt(num);
            double _real,_imag;
            _real = result.real();
            _imag = result.imag();
            Num *res_r = create_Float(_real,0),*res_i = create_Float(_imag,0);
            res_r->exact2un(), res_i->exact2un();
            return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));
        }
    }
};

class Exp: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (numlk->value->tplvl<3)
        {
            double tmp = f_from_tplvl(numlk->value);
            tmp = exp (tmp);
            Num *res = create_Float(tmp);
            res->exact2un();
            res->Q2CrQ();
            return res;
        }
        else
        {
            Cmplx *opr = SCAST_CMPLX(numlk->value);
            complex<double> num(f_from_tplvl(opr->real),f_from_tplvl(opr->imag));
            complex<double> result = exp(num);
            double _real,_imag;
            _real = result.real();
            _imag = result.imag();
            Num *res_r = create_Float(_real,0),*res_i = create_Float(_imag,0);
            res_r->exact2un(), res_i->exact2un();
            return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));
        }
    }
};

class Expt: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (numlk->is_all_less_type(2))
        {

            double tmp1 = f_from_tplvl(numlk->value);
            double tmp2 = f_from_tplvl(numlk->next->value);
            if (tmp1 < 0)
            {
                Num *res;
                complex <double> di(-1,0);
                complex <double> mi(tmp2,0);
                di = pow(di,mi);
                if(di.imag()==0)    res = create_Float(pow(-1*tmp1,tmp2)*di.real());
                else res = create_Cmplx(SCAST_R(create_Float(pow(-1*tmp1,tmp2)*di.real())),SCAST_R(create_Float(pow(-1*tmp1,tmp2)*di.imag())));
                res->exact2un();
                res->Q2CrQ();
                return res;

            }
            else
            {
                double tmp = pow (tmp1,tmp2);
                Num *res = create_Float(tmp);
                res->exact2un();
                res->Q2CrQ();
                return res;
            }

        }
        else
        {
            Cmplx *a = new Cmplx(0,0);
            Cmplx *opr = SCAST_CMPLX(a->convert((numlk->value)));
            complex<double> num(f_from_tplvl(opr->real),f_from_tplvl(opr->imag));
            numlk = numlk->next;
            Cmplx *opr1 = SCAST_CMPLX(a->convert((numlk->value)));
            complex<double> num1(f_from_tplvl(opr1->real),f_from_tplvl(opr1->imag));
            complex<double> result = pow(num, num1);
            double _real,_imag;
            _real = result.real();
            _imag = result.imag();
            Num *res_r = create_Float(_real,0),*res_i = create_Float(_imag,0);
            res_r->exact2un(), res_i->exact2un();
            return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));
        }
    }
};

class Ex2un: public Opt
{
      Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num* res = numlk->value;
        if (res->tplvl == 1) res = create_Float(SCAST_RATIONAL(res)->x);
        res->exact2un();
        return res;
    }
};

class Ex2ex: public Opt
{
      Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num* res = numlk->value;
        ///alarm!!!unsure!!!attention!!!
        if (numlk->value->exact == 0 && !sanguan )
        {
            double x = f_from_tplvl(numlk->value);
            int zs = x - (long long )x;
            x = x - zs;
            Longint tmp_fz =(long long)(x*pow(2,53)) ;
            Longint tmp_fm = (long long) 1<<53;
            res =  create_Rational(tmp_fz+Longint(zs)*tmp_fm,tmp_fm);
        }
        res->exact2ex();
        if (res->type == 0) res->CrQ2Q();
        return res;
    }
};

Num* Dec2other(Num *opr, int base)// 进制转换
{
    if (opr->tplvl == 2)
            {
                Longint tmp_fz,tmp_fm;
                F2R(SCAST_FLOAT(opr)->x,tmp_fz,tmp_fm);
                Num* old = opr;
                opr = create_Rational(tmp_fz,tmp_fm);
                delete old;
            }

            switch (opr->tplvl)
            {
            case 0 :
                return new Int(fromDec(SCAST_INT(opr)->x,base));
                break;

            case 1:
                Longint tmp_fz = SCAST_RATIONAL(opr)->fenzi, tmp_fm = SCAST_RATIONAL(opr)->fenmu;
                tmp_fz = fromDec(tmp_fz,base), tmp_fm = fromDec(tmp_fm,base);
                return create_Rational(tmp_fz, tmp_fm);
                break;
            }
}

class Num2str: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        Num* opr = numlk->value;
        if (numlk->get_len() == 1)
        {
            return new String (opr->toString());
        }
        numlk = numlk->next;
        Num* radix = numlk->value;
        if (radix->tplvl != 0) throw SecW();
        if (SCAST_INT(radix)->x != 2 && SCAST_INT(radix)->x != 8 && SCAST_INT(radix)->x != 10 && SCAST_INT(radix)->x !=16) throw SecW();
        int base = Longint2double(SCAST_INT(radix)->x);
        if (base != 10)
        {
            if (opr->exact != 1) throw TypeW();
            if (opr->tplvl != 3)
            {
                return new String(Dec2other(opr,base)->toString());
            }
            else
            {
                Num* _real = SCAST_CMPLX(opr)->real, *_imag = SCAST_CMPLX(opr)->imag;
                Num *real = Dec2other(_real,base), *imag = Dec2other(_imag,base);
                Num * res = create_Cmplx(SCAST_R(real),SCAST_R(imag));
                return new String(res->toString());

            }

        }
        else
        {
            return new String (opr->toString());
        }



    }
};

class Str2num: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->value->tplvl != -2) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();

        Num *res = NULL;
        string tmp = SCAST_S(numlk->value)->str;
        low_down(tmp);
        if (numlk->get_len() == 2)
        {
            numlk = numlk->next;
            Num* radix = numlk->value;
            //if (numlk->value->tplvl != -2) throw TypeW();
            if (radix->tplvl != 0) throw SecW();
            if (SCAST_INT(radix)->x != 2 && SCAST_INT(radix)->x != 8 && SCAST_INT(radix)->x != 10 && SCAST_INT(radix)->x !=16) throw SecW();
            int base = Longint2double(SCAST_INT(radix)->x);
            if(base == 2) if (tmp.substr(0,2) != "#b")tmp = "#b"+tmp;
            if(base == 8) if (tmp.substr(0,2) != "#o")tmp = "#o"+tmp;
            if(base == 16) if (tmp.substr(0,2) != "#x")tmp = "#x"+tmp;

        }
        res = Int::from_str(tmp);
        if (!res) res = Rational::from_str(tmp);
        if (!res) res = Cmplx :: from_str(tmp);
        if (!res) return new Bool(0);
        else return res;
    }
};

class Isbool:public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num *t1 = numlk->value;
        Num *res;
        if (t1->tplvl!= -1)
            res = new Bool(0);
        else
            res = new Bool(1);

        return res;

    }
};

class NewL :public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() > 0 ) throw Ag2many();
        cout << endl;
        Num *res = new String("");
        res->dsply = Num::YES;
        return res;
    }

};
class Max : public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (!numlk->is_all_less_type(2)) throw NOcpCMP();
        bool fe = numlk->is_exact();
        int fq = numlk->is_Q();
        double fir,a1,a2;
        Num *res = numlk->value,*old;

        numlk = numlk->next;
        for (; numlk; numlk = numlk->next)
        {
            Num *opr = numlk->value;
            old = res;
            a1 = f_from_tplvl(opr);
            a2 = f_from_tplvl(res);

            res = (a1 > a2) ?  opr:res;

            if (a1>a2) delete old;

        }

        if (!fe) res->exact2un();
        else res->exact2ex();
        if (!fq) res->Q2CrQ();
        return res;
    }
};

class Min : public Opt
{
  Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (!numlk->is_all_less_type(2)) throw NOcpCMP();
        bool fe = numlk->is_exact();
        bool f = bool(numlk->value->exact);
        int fq = numlk->is_Q();
        double fir,a1,a2;
        Num *res = numlk->value,*old;

        numlk = numlk->next;
        for (; numlk; numlk = numlk->next)
        {
            Num *opr = numlk->value;
            old = res;
            a1 = f_from_tplvl(opr);
            a2 = f_from_tplvl(res);

            res = (a1 < a2) ?  opr:res;

            if (a1 < a2) delete old;
        }

        if (!fe) res->exact2un();
        else res->exact2ex();
        if (!fq) res->Q2CrQ();
        return res;
    }
};

class Abs : public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (!numlk->is_all_less_type(2)) throw TypeW();
        bool f = numlk->is_exact();
        bool fq = numlk->is_Q();
        int tp = numlk->value->type;
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num *res = numlk->value;
        if (numlk->value->tplvl == 0 ) res =  new Int (abs(SCAST_INT(numlk->value)->x));

        if (numlk->value->tplvl == 1 )
        {

            if (SCAST_RATIONAL(res)->fenzi < 0) SCAST_RATIONAL(res)->fenzi = -(SCAST_RATIONAL(res)->fenzi),SCAST_RATIONAL(res)->x = -(SCAST_RATIONAL(res)->x);

        }
        if (numlk->value->tplvl == 2 )
            if (SCAST_FLOAT(res)->x < 0)  SCAST_FLOAT(res)->x = -(SCAST_FLOAT(res)->x);

        if (!f) res->exact2un();
        if (!fq) res->Q2CrQ();

        return res;
    }
};

class Mgntd : public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (!numlk->is_all_less_type(3)) throw TypeW();
        bool f = numlk->is_exact();
        bool fq = numlk->is_Q();
        int tp = numlk->value->type;
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (numlk->value->tplvl<3)
        {
            double tmp = f_from_tplvl(numlk->value);
            tmp = abs (tmp);
            Num *res = create_Float(tmp,tp),*old;
            if (!f) res->exact2un();
            if (!fq) res->Q2CrQ();
            return res;
        }
        else
        {
            Cmplx *opr = SCAST_CMPLX(numlk->value);
            complex<double> num(f_from_tplvl(opr->real),f_from_tplvl(opr->imag));
            complex<double> result = abs(num);
            double _real,_imag;
            _real = result.real();
            _imag = result.imag();
            Num *res_r = create_Float(_real,0),*res_i = create_Float(_imag,0);
            res_r->exact2un(), res_i->exact2un();
            return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));
        }

    }
};

class MkRt: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (!numlk->is_all_less_type(2)) throw TypeW();
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        Num *res_r = numlk->value;
        numlk = numlk->next;
        Num *res_i = numlk->value;
        return create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));
    }
};

class MkPl: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        Num *len = numlk->value;
        numlk = numlk->next;
        Num *angle = numlk->value;
        double _real,_imag;
        _real = f_from_tplvl(len) * cos(f_from_tplvl(angle));
        _imag = f_from_tplvl(len) * sin(f_from_tplvl(angle));
        bool flgR = 1, flgI = 1;
        if (_real == 0) flgR = 0;
        if (_imag == 0) flgI = 0;
        Num *res_r,*res_i;
        res_r = create_Float(_real,1);
        if (!flgR) res_r->exact2ex();
        res_i = create_Float(_imag,1);
        if (!flgI) res_i->exact2ex();
        Num* res = create_Cmplx(SCAST_R(res_r),SCAST_R(res_i));
        if (!flgI || !flgR) res->exact2ex();
        return res;
    }
};

class FindR: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (numlk->value->tplvl<3)
        {
            return numlk->value;
        }
        else
        {
            return SCAST_N(SCAST_CMPLX(numlk->value)->real);
        }
    }
};

class FindI: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (numlk->value->tplvl<3)
        {
            return new Int(0);
        }
        else
        {
            return SCAST_N(SCAST_CMPLX(numlk->value)->imag);
        }
    }
};

class FindA: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (numlk->value->tplvl<3)
        {
            double tmp;
            tmp = (f_from_tplvl(numlk->value) >= 0)? 0 : acos(-1);
            Num* res = create_Float(tmp);
            res->exact2un();
            return res;
        }
        else
        {
            Num *res;
            Cmplx *tmp = SCAST_CMPLX(numlk->value);
            RlNum *x = tmp->imag,*y = tmp->real;
            if (f_from_tplvl(x) == 0 && f_from_tplvl(y) == 0) throw NOallZero();
            if (f_from_tplvl(y) == 0 && f_from_tplvl(x) > 0) res = create_Float(acos(-1)/2);
            else if (f_from_tplvl(y) == 0 && f_from_tplvl(x) < 0) res = create_Float(-acos(-1)/2);
            else if (f_from_tplvl(x) == 0 && f_from_tplvl(y) > 0) res = create_Float(0.0);
            else if (f_from_tplvl(x) == 0 && f_from_tplvl(y) < 0) res = create_Float(acos(-1));
            else
            {
                double tmp = atan(f_from_tplvl(x)/f_from_tplvl(y));
                if (f_from_tplvl(y) < 0)
                {
                    tmp += acos(-1);
                    if (tmp >= acos(-1)) tmp -= 2*acos(-1);
                }
                res = create_Float(tmp);
            }

            res->exact2un();
            return res;
        }
    }
};



class IsEqual:public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        bool f = bool(numlk->value->exact);
        Num *t1 = numlk->value;

        numlk = numlk->next;

        Num *t2 = numlk->value;

        Num *res;
        if (t1->tplvl == -2 && t2->tplvl == -2)
        {
            if (SCAST_S(t1)->str != SCAST_S(t2)->str)
                res = new Bool(0);
            else
                res = new Bool(1);
        }
        else if (t1->tplvl == 3 && t2->tplvl == 3)
        {
            RlNum * _r1 = SCAST_CMPLX(t1)->real ,* _i1 = SCAST_CMPLX(t1)->imag ,* _r2 = SCAST_CMPLX(t2)->real ,* _i2 = SCAST_CMPLX(t2)->imag;
            if ( _r2->exact != _r1->exact || f_from_tplvl(_r2)!=f_from_tplvl(_r1) ||   _i2->exact != _i1->exact || f_from_tplvl(_i2)!=f_from_tplvl(_i1))
                res = new Bool(0);
            else
                res = new Bool(1);
        }
        else{
        if (t2->exact != t1->exact || f_from_tplvl(t1)!=f_from_tplvl(t2))
            res = new Bool(0);
        else
            res = new Bool(1);
        }

        return res;

    }
};

class IsNum:public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num *t1 = numlk->value;
        Num *res;
        if (t1->tplvl > 3 || t1->tplvl < 0)
            res = new Bool(0);
        else
            res = new Bool(1);

        return res;

    }
};

class IsReal:public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num *t1 = numlk->value;
        Num *res;
        if (t1->tplvl > 2 || t1->tplvl < 0)
            res = new Bool(0);
        else
            res = new Bool(1);

        return res;

    }
};

class IsRational:public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num *t1 = numlk->value;
        Num *res;
        if (t1->tplvl > 1 || t1->tplvl < 0|| t1->type != 1)
            res = new Bool(0);
        else
            res = new Bool(1);

        return res;


    }
};

class IsInteger:public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num *t1 = numlk->value;
        Num *res;
        if (t1->tplvl != 0 )
            res = new Bool(0);
        else
            res = new Bool(1);

        return res;


    }
};

class IsComplex:public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num *t1 = numlk->value;
        Num *res;
        if (t1->tplvl > 3 || t1->tplvl < 0)
            res = new Bool(0);
        else
            res = new Bool(1);
        return res;
    }
};

class IsExact:public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num *t1 = numlk->value;
        Num *res;
        if (t1->exact != Num::EXACT)
            res = new Bool(0);
        else
            res = new Bool(1);

        return res;
    }
};

class IsInexact:public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num *t1 = numlk->value;
        Num *res;
        if (t1->exact != Num::UNEXACT)
            res = new Bool(0);
        else
            res = new Bool(1);

        return res;
    }


};

class Quotient:public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        Num *t1 = numlk->value;
        numlk = numlk->next;
        Num *t2 = numlk->value;
        Num *res;
        if (t1->tplvl != Num::INT) throw FirW();
        if (t2->tplvl != Num::INT) throw SecW();
        double n1 = f_from_tplvl(t1), n2 = f_from_tplvl(t2);
        if (n2 ==0) throw SecNOzero();
        Longint tmp;
        tmp = SCAST_INT(t1)->x / SCAST_INT(t2)->x;
        res = new Int(tmp);

        return res;

    }
};

class Remainder:public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        Num *t1 = numlk->value;
        numlk = numlk->next;
        Num *t2 = numlk->value;
        Num *res;
        if (t1->tplvl != Num::INT) throw FirW();
        if (t2->tplvl != Num::INT) throw SecW();
        double n1 = f_from_tplvl(t1), n2 = f_from_tplvl(t2);
        if (n2 ==0) throw SecNOzero();
        Longint tmp;
        tmp = SCAST_INT(t1)->x % SCAST_INT(t2)->x;
        res = new Int(tmp);

        return res;

    }
};

class Modulo:public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        Num *t1 = numlk->value;
        numlk = numlk->next;
        Num *t2 = numlk->value;
        Num *res;
        if (t1->tplvl != Num::INT) throw FirW();
        if (t2->tplvl != Num::INT) throw SecW();
        double n1 = f_from_tplvl(t1), n2 = f_from_tplvl(t2);
        if (n2 ==0) throw SecNOzero();
        Longint tmp;
        tmp = tmp = SCAST_INT(t1)->x % SCAST_INT(t2)->x;
        if (n1*n2 >= 0)
        res = new Int(tmp);
        else res = new Int(tmp+SCAST_INT(t2)->x);

        return res;

    }
};

class Gcd : public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if  (!numlk->is_all_less_type(0)) throw NOallInt();
        bool flg = numlk->is_exact();
        Num *t1 = numlk->value;
        if (numlk->get_len() == 0) return new Int(0);
        if (numlk->get_len() == 1) return new Int(SCAST_INT(t1)->x);
        numlk = numlk->next;
        Num *t2 = numlk->value;
        Num *res;

        Longint n1 = abs(SCAST_INT(t1)->x), n2 = abs(SCAST_INT(t2)->x);
        Longint tmp;
        tmp = gcd (n1, n2);
        for (numlk = numlk->next; numlk; numlk = numlk->next)
        {
            Num *opr = numlk->value;
            tmp = gcd (tmp, abs(SCAST_INT(opr)->x));
        }
        res = new Int(tmp);
        if (!flg) res->exact2un();

        return res;
    }
};

class Lcm : public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if  (!numlk->is_all_less_type(0)) throw NOallInt();
        bool flg = numlk->is_exact();
        Num *t1 = numlk->value;
        if (numlk->get_len() == 0) return new Int(1);
        if (numlk->get_len() == 1) return new Int(SCAST_INT(t1)->x);
        numlk = numlk->next;
        Num *t2 = numlk->value;
        Num *res;
        Longint n1 = abs(SCAST_INT(t1)->x), n2 = abs(SCAST_INT(t2)->x);
        Longint tmp;
        tmp = lcm (n1, n2);
        for (numlk = numlk->next; numlk; numlk = numlk->next)
        {
            Num *opr = numlk->value;
            tmp = lcm (tmp, abs(SCAST_INT(opr)->x));

        }

        res = new Int(tmp);
        if (!flg ) res->exact2un();

        return res;
    }

};

class Numerator : public Opt
{
    Num* calc(Numlks *numlk)
    {
        bool ext = 1;
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num *t1 = numlk->value, *res;
        if(t1->type != 1 ) throw NOCrQ2Q();
        Longint tmp,a;
        if (t1->tplvl == 0) tmp = SCAST_INT(t1)->x;
        if (t1->exact == 0&& !sanguan)
        {

            double x = f_from_tplvl(t1);
            int zs = x - (long long )x;
            x = x - zs;
            Longint tmp_fz =(long long)(x*pow(2,53));
            Longint tmp_fm = (long long) 1<<53;
            tmp = (tmp_fz+Longint(zs)*tmp_fm);
            tmp = tmp / gcd(tmp,tmp_fm);
            ext = 0;


        }
        else if (t1->tplvl == 1) tmp = SCAST_RATIONAL(t1)->fenzi;
        else if (t1->tplvl == 2) F2R(f_from_tplvl(t1),tmp,a);
        else if (t1->tplvl == 3) throw NOCp2R();
        res = new Int(tmp);
        if (!ext) res->exact2un();
        return res;
    }

};

class Denominator : public Opt
{
    Num* calc(Numlks *numlk)
    {
        bool ext = 1;
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num *t1 = numlk->value, *res;
        if(t1->type != 1 ) throw NOCrQ2Q();
        Longint tmp,a;
        if (t1->tplvl == 0) tmp = 1;
        if (t1->exact == 0&& !sanguan)
        {

            double x = f_from_tplvl(t1);
            int zs = x - (long long )x;
            x = x - zs;
            Longint tmp_fz =(long long)(x*pow(2,53));
            Longint tmp_fm = (long long) 1<<53;
            tmp = tmp_fm / gcd(tmp_fz,tmp_fm);
            ext = 0;

        }
        else if (t1->tplvl == 1) tmp = SCAST_RATIONAL(t1)->fenmu;
        else if (t1->tplvl == 2) F2R(f_from_tplvl(t1),a,tmp);
        else if (t1->tplvl == 3) throw NOCp2R();
        res = new Int(tmp);
        if (!ext) res->exact2un();
        return res;
    }

};

class Floor : public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num *t1 = numlk->value, *res;
        if (t1->tplvl > 2) throw NOCp2R();
        Longint tmp;
        double tmp1;
        if (t1->tplvl == 0) tmp = (SCAST_INT(t1)->x),res = new Int(tmp);
        else if (t1->tplvl == 1)
        {
            tmp = (SCAST_RATIONAL(t1)->fenzi /SCAST_RATIONAL(t1)->fenmu);
            if (f_from_tplvl(t1) < 0 ) tmp -= Longint(1);
            res = new Int(tmp);
        }
        else if (t1->tplvl == 2) tmp1 = floor(f_from_tplvl(t1)),res = create_Float(tmp1);

        return res;
    }

};

class Ceiling : public Opt
{
    Num* calc(Numlks *numlk)
    {
       if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num *t1 = numlk->value, *res;
        if (t1->tplvl > 2) throw NOCp2R();
        Longint tmp;
        double tmp1;
        if (t1->tplvl == 0) tmp = (SCAST_INT(t1)->x),res = new Int(tmp);
        else if (t1->tplvl == 1)
        {
            tmp = (SCAST_RATIONAL(t1)->fenzi /SCAST_RATIONAL(t1)->fenmu);
            if (f_from_tplvl(t1) > 0 ) tmp += Longint(1);
            res = new Int(tmp);
        }
        else if (t1->tplvl == 2) tmp1 = floor(f_from_tplvl(t1)),res = create_Float(tmp1);

        return res;
    }

};

class Round : public Opt
{
    Num* calc(Numlks *numlk)
    {
       if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num *t1 = numlk->value, *res;
        if (t1->tplvl > 2) throw NOCp2R();
        Longint tmp;
        double tmp1;
        if (t1->tplvl == 0) tmp = (SCAST_INT(t1)->x),res = new Int(tmp);
        else if (t1->tplvl == 1)
        {
            Longint tmp_fz = SCAST_RATIONAL(t1)->fenzi, tmp_fm  = SCAST_RATIONAL(t1)->fenmu;
            bool tmp_sn = tmp_fz.sign;
            tmp_fz = abs(tmp_fz);
            Longint rmder = tmp_fz % tmp_fm;
            tmp = tmp_fz / tmp_fm;

            if (rmder * Longint(2) == tmp_fm)
            {
                if (tmp % Longint(2) == 1) tmp += 1;
            }
            if (rmder * Longint(2) > tmp_fm) tmp += 1;


            res = new Int(tmp);
        }
        else if (t1->tplvl == 2 && abs((int)f_from_tplvl(t1) - f_from_tplvl(t1) +0.5)== 0)
            tmp1 = round (f_from_tplvl(t1)/2.0) *2, res = create_Float(tmp1);
        else  tmp1 = round(f_from_tplvl(t1)),res = create_Float(tmp1);

        return res;
    }

};

class Truncate : public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num *t1 = numlk->value, *res;
        if (t1->tplvl > 2) throw NOCp2R();
        Longint tmp;
        double tmp1;
        if (t1->tplvl == 0) tmp = (SCAST_INT(t1)->x),res = new Int(tmp);
        else if (t1->tplvl == 1)
        {
            tmp = (SCAST_RATIONAL(t1)->fenzi /SCAST_RATIONAL(t1)->fenmu);
            res = new Int(tmp);
        }
        else if (t1->tplvl == 2) tmp1 = floor(f_from_tplvl(t1)),res = create_Float(tmp1);

        return res;
    }

};

class Equal : public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 2 ) throw Ag2less();
        Num *pre = numlk->value;
        bool f = true;
        numlk = numlk->next;
        for (; numlk; numlk = numlk->next)
        {
            Num *opr = numlk->value;
            if (f_from_tplvl(opr) != f_from_tplvl(pre))
            {f = 0; break;}
            pre = opr;
        }
        Num *res = new Bool(f);
        return res;
    }

};

class Less : public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (!numlk->is_all_less_type(2)) throw NOcpCMP();
        Num *pre = numlk->value;
        bool f = true;
        numlk = numlk->next;
        for (; numlk; numlk = numlk->next)
        {
            Num *opr = numlk->value;
            if ( f_from_tplvl(opr) > f_from_tplvl(pre))
            {f = 0; break;}
            pre = opr;
        }
        Num *res = new Bool(f);
        return res;
    }

};

class Larger : public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (!numlk->is_all_less_type(2)) throw NOcpCMP();
        Num *pre = numlk->value;
        bool f = true;
        numlk = numlk->next;
        for (; numlk; numlk = numlk->next)
        {
            Num *opr = numlk->value;
            if (f_from_tplvl(opr) < f_from_tplvl(pre))
            {f = 0; break;}
            pre = opr;
        }
        Num *res = new Bool(f);
        return res;
    }

};

class SLess : public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (!numlk->is_all_less_type(2)) throw NOcpCMP();
        Num *pre = numlk->value;
        bool f = true;
        numlk = numlk->next;
        for (; numlk; numlk = numlk->next)
        {
            Num *opr = numlk->value;
            if (f_from_tplvl(opr) >= f_from_tplvl(pre))
            {f = 0; break;}
            pre = opr;
        }
        Num *res = new Bool(f);
        return res;
    }

};

class SLarger : public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (!numlk->is_all_less_type(2)) throw NOcpCMP();
        Num *pre = numlk->value;
        bool f = true;
        numlk = numlk->next;
        for (; numlk; numlk = numlk->next)
        {
            Num *opr = numlk->value;
            if (f_from_tplvl(opr) <= f_from_tplvl(pre))
            {f = 0; break;}
            pre = opr;
        }
        Num *res = new Bool(f);
        return res;
    }

};

class IsZero: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num *opr = numlk->value;
        bool f =false;
        if (opr->tplvl == 3)
        {
            RlNum *r = SCAST_CMPLX(opr)->real ,*i = SCAST_CMPLX(opr)->imag;
            if (f_from_tplvl(r) == 0 && f_from_tplvl(i) == 0 && r->tplvl == 0 && i->tplvl == 0  && r->type == 1 && i-> type == 1)
                f = 1;
        }
        else
        {
            if (opr-> type == 1 && opr->tplvl <=1 && f_from_tplvl(opr) == 0)
                f = 1;
        }
        Num *res = new Bool(f);
        return res;

    }
};

class IsPositive: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (!numlk->is_all_less_type(2)) throw NOcpCMP();
        bool f = (f_from_tplvl(numlk->value) > 0);
        Num *res = new Bool (f);
        return res;
    }
};

class IsNegative: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (!numlk->is_all_less_type(2)) throw NOcpCMP();
        bool f = (f_from_tplvl(numlk->value) < 0);
        Num *res = new Bool (f);
        return res;
    }
};

class IsOdd: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (!numlk->is_all_less_type(0)) throw OnlyInt();
        bool f = 1;
        if (SCAST_INT(numlk->value)->x % 2 == 0)
            f =0;
        Num *res = new Bool (f);
        return res;
    }
};

class IsEven: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_higher_type(0)) throw TypeW();
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (!numlk->is_all_less_type(0)) throw OnlyInt();
        bool f = 1;
        if (SCAST_INT(numlk->value)->x % 2 == 0)
            f =0;
        Num *res = new Bool (1-f);
        return res;
    }
};

class Anti:public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (numlk->value->tplvl != -1) return new Bool(0);
        else
        {
            if(SCAST_B(numlk->value)->x == 1) return new Bool(0);
            else return new Bool(1);
        }
    }
};

class IsString :public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        return new Bool(numlk->value->tplvl == -2) ;
    }
};

class Mkstr : public Opt
{
    Num* calc(Numlks *numlk)
    {
        int len = numlk->get_len();
        if (len < 1 ) throw Ag2less();
        if (len > 2 ) throw Ag2many();
        char add;
        string tmp = "";
        if (numlk->value->tplvl != 0 || SCAST_INT(numlk->value)->x < Longint(0) || SCAST_INT(numlk->value)->exact == 0) throw OnlyE_NN_Int();
        Longint times = SCAST_INT(numlk->value)->x;
        numlk = numlk->next;
        if (len == 1) add = ' ';
        else
        {
            if (numlk->value->tplvl != -3) throw TypeW();
            add = SCAST_CH(numlk->value)->chr;
        }
        while(times != Longint(0))
        {
            tmp += add;
            times -= Longint(1);
        }
        return new String (tmp);

    }
};

class Strlen :public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (numlk->value->tplvl != -2)  throw TypeW();
        return new Int((SCAST_S(numlk->value)->str).length());
    }

};

class StrRf :public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (numlk->value->tplvl != -2)  throw TypeW();
        string tmp = SCAST_S(numlk->value)->str;
        numlk = numlk->next;
        if (numlk->value->tplvl != 0 || SCAST_INT(numlk->value)->x < Longint(0) || SCAST_INT(numlk->value)->exact == 0) throw OnlyE_NN_Int();
        int index = 0;
        Longint times = SCAST_INT(numlk->value)->x;
        while(times != Longint(0))
        {
            index++;
            times -= Longint(1);
        }
        return new Char(tmp[index]);
    }
};

class SIsequal: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-2)) throw TypeW();
        string fir = SCAST_S(numlk->value)->str;
        numlk = numlk->next;
        string sec = SCAST_S(numlk->value)->str;
        return new Bool (fir == sec);
    }
};

class ScIsequal: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-2)) throw TypeW();
        string fir = SCAST_S(numlk->value)->str;
        numlk = numlk->next;
        string sec = SCAST_S(numlk->value)->str;
        low_down(fir);
        low_down(sec);
        return new Bool (fir == sec);
    }
};

class SIsSless: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-2)) throw TypeW();
        string fir = SCAST_S(numlk->value)->str;
        numlk = numlk->next;
        string sec = SCAST_S(numlk->value)->str;
        return new Bool (fir < sec);
    }
};

class SIsSlarger: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-2)) throw TypeW();
        string fir = SCAST_S(numlk->value)->str;
        numlk = numlk->next;
        string sec = SCAST_S(numlk->value)->str;
        return new Bool (fir > sec);
    }
};

class SIsless: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-2)) throw TypeW();
        string fir = SCAST_S(numlk->value)->str;
        numlk = numlk->next;
        string sec = SCAST_S(numlk->value)->str;
        return new Bool (fir <= sec);
    }
};

class SIslarger: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-2)) throw TypeW();
        string fir = SCAST_S(numlk->value)->str;
        numlk = numlk->next;
        string sec = SCAST_S(numlk->value)->str;
        return new Bool (fir >= sec);
    }
};

class ScIsSless: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-2)) throw TypeW();
        string fir = SCAST_S(numlk->value)->str;
        numlk = numlk->next;
        string sec = SCAST_S(numlk->value)->str;
        low_down(fir);
        low_down(sec);
        return new Bool (fir < sec);
    }
};

class ScIsSlarger: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-2)) throw TypeW();
        string fir = SCAST_S(numlk->value)->str;
        numlk = numlk->next;
        string sec = SCAST_S(numlk->value)->str;
        low_down(fir);
        low_down(sec);
        return new Bool (fir > sec);
    }
};

class ScIsless: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-2)) throw TypeW();
        string fir = SCAST_S(numlk->value)->str;
        numlk = numlk->next;
        string sec = SCAST_S(numlk->value)->str;
        low_down(fir);
        low_down(sec);
        return new Bool (fir <= sec);
    }
};

class ScIslarger: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-2)) throw TypeW();
        string fir = SCAST_S(numlk->value)->str;
        numlk = numlk->next;
        string sec = SCAST_S(numlk->value)->str;
        low_down(fir);
        low_down(sec);
        return new Bool (fir >= sec);
    }
};

class StrApp: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (!numlk->is_all_type(-2)) throw TypeW();
        string result = "";
        for (;numlk;numlk = numlk->next)
        {
            result += SCAST_S(numlk->value)->str;
        }
        return new String(result);
    }
};

class Substr: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 3 ) throw Ag2less();
        if (numlk->get_len() > 3 ) throw Ag2many();
        if (numlk->value->tplvl != -2)  throw TypeW();
        string tmp = SCAST_S(numlk->value)->str;
        numlk = numlk->next;
        if (numlk->value->tplvl != 0 || SCAST_INT(numlk->value)->x < Longint(0) || SCAST_INT(numlk->value)->exact == 0) throw OnlyE_NN_Int();
        int index = 0;
        Longint times = SCAST_INT(numlk->value)->x;
        while(times != Longint(0))
        {
            index++;
            times -= Longint(1);
        }
        numlk = numlk->next;
        if (numlk->value->tplvl != 0 || SCAST_INT(numlk->value)->x < Longint(0) ||SCAST_INT(numlk->value)->exact == 0) throw OnlyE_NN_Int();
        int index1 = 0;
        Longint times1 = SCAST_INT(numlk->value)->x;
        while(times1 != Longint(0))
        {
            index1++;
            times1 -= Longint(1);
        }

        if (index1 < index || index > tmp.length() || index1 > tmp.length()) throw IdxW();
        return new String(tmp.substr(index,index1));
    }
};


class StrCpy: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (numlk->value->tplvl != -2)  throw TypeW();
        return new String(SCAST_S(numlk->value)->str);
    }
};

class Display: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        Num* res = numlk->value;
        res->C2DS();
        res->display();
        cout << flush;
        return res;
    }
};

class IsChar :public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        return new Bool(numlk->value->tplvl == -3) ;
    }
};

class CIsequal: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        char fir = SCAST_CH(numlk->value)->chr;
        numlk = numlk->next;
        char sec = SCAST_CH(numlk->value)->chr;
        return new Bool (fir == sec);
    }
};

class CcIsequal: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        char fir = SCAST_CH(numlk->value)->chr;
        numlk = numlk->next;
        char sec = SCAST_CH(numlk->value)->chr;
        low_down(fir);
        low_down(sec);
        return new Bool (fir == sec);
    }
};

class CIsSless: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        char fir = SCAST_CH(numlk->value)->chr;
        numlk = numlk->next;
        char sec = SCAST_CH(numlk->value)->chr;
        return new Bool (fir < sec);
    }
};

class CIsSlarger: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        char fir = SCAST_CH(numlk->value)->chr;
        numlk = numlk->next;
        char sec = SCAST_CH(numlk->value)->chr;
        return new Bool (fir > sec);
    }
};

class CIsless: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        char fir = SCAST_CH(numlk->value)->chr;
        numlk = numlk->next;
        char sec = SCAST_CH(numlk->value)->chr;
        return new Bool (fir <= sec);
    }
};

class CIslarger: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        char fir = SCAST_CH(numlk->value)->chr;
        numlk = numlk->next;
        char sec = SCAST_CH(numlk->value)->chr;
        return new Bool (fir >= sec);
    }
};

class CcIsSless: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        char fir = SCAST_CH(numlk->value)->chr;
        numlk = numlk->next;
        char sec = SCAST_CH(numlk->value)->chr;
        low_down(fir);
        low_down(sec);
        return new Bool (fir < sec);
    }
};

class CcIsSlarger: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        char fir = SCAST_CH(numlk->value)->chr;
        numlk = numlk->next;
        char sec = SCAST_CH(numlk->value)->chr;
        low_down(fir);
        low_down(sec);
        return new Bool (fir > sec);
    }
};

class CcIsless: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        char fir = SCAST_CH(numlk->value)->chr;
        numlk = numlk->next;
        char sec = SCAST_CH(numlk->value)->chr;
        low_down(fir);
        low_down(sec);
        return new Bool (fir <= sec);
    }
};

class CcIslarger: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 2 ) throw Ag2less();
        if (numlk->get_len() > 2 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        char fir = SCAST_CH(numlk->value)->chr;
        numlk = numlk->next;
        char sec = SCAST_CH(numlk->value)->chr;
        low_down(fir);
        low_down(sec);
        return new Bool (fir >= sec);
    }
};

class CIsAphb: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        char tmp_ch = SCAST_CH(numlk->value)->chr;
        low_down(tmp_ch);
        if (tmp_ch <= 'z' && tmp_ch >= 'a') return new Bool (1);
        else return new Bool (0);
    }
};

class CIsNum: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        char tmp_ch = SCAST_CH(numlk->value)->chr;
        if (tmp_ch <= '9' && tmp_ch >= '0') return new Bool (1);
        else return new Bool (0);
    }
};

class CIsWs: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        char tmp_ch = SCAST_CH(numlk->value)->chr;
        if (tmp_ch == ' ' || tmp_ch == '\t' || tmp_ch == '\n' || tmp_ch == 11 || tmp_ch == 32
            || tmp_ch == 9 || tmp_ch == 12 || tmp_ch == 10 || tmp_ch == 13) return new Bool (1);
        else return new Bool (0);
    }
};

class CIsUp: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        char tmp_ch = SCAST_CH(numlk->value)->chr;
        if (tmp_ch <= 'Z' && tmp_ch >= 'A') return new Bool (1);
        else return new Bool (0);
    }
};

class CIsDwn: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        char tmp_ch = SCAST_CH(numlk->value)->chr;
        if (tmp_ch <= 'z' && tmp_ch >= 'a') return new Bool (1);
        else return new Bool (0);
    }
};

class Cup: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        Num *res = numlk->value;
        char tmp_ch = SCAST_CH(numlk->value)->chr;
        if (tmp_ch <= 'z' && tmp_ch >= 'a') {delete res; return new Char (char(tmp_ch - 32));}
        else return res;
    }
};

class Cdown: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        Num *res = numlk->value;
        char tmp_ch = SCAST_CH(numlk->value)->chr;
        if (tmp_ch <= 'Z' && tmp_ch >= 'A') {delete res; return new Char (char(tmp_ch + 32));}
        else return res;
    }
};

class C2Int: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (!numlk->is_all_type(-3)) throw TypeW();
        int tmp = (int)SCAST_CH(numlk->value)->chr;
        return new Int (Longint(tmp));
    }

};

class Int2C: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        if (!numlk->is_all_type(0)) throw TypeW();
        char tmp_ch = (int) Longint2double(SCAST_INT(numlk->value)->x);
        return new Char (tmp_ch);
    }

};

class MkStr: public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (!numlk->is_all_type(-3)) throw TypeW();
        Num *res;
        string tmp = "";
        char ch;
        for (;numlk;numlk = numlk->next)
        {
            ch = SCAST_CH(numlk->value)->chr;
            tmp += ch;
        }
        return new String (tmp);
    }
};

class IsProd :public Opt
{
    Num* calc(Numlks *numlk)
    {
        if (numlk->get_len() < 1 ) throw Ag2less();
        if (numlk->get_len() > 1 ) throw Ag2many();
        return new Bool(numlk->value->tplvl == -4) ;
    }
};

//==输入流==========================
FILE *input = stdin;
const long long  MAX_BUFF = 1<<20+1;
char bfr[MAX_BUFF], *bptr = bfr;

string next_token()
{
    static bool bkflg = 0;
    char *res = NULL;
    int ch;
    while (!res)
    {
        if (bptr > bfr && (*bfr == '(' || *bfr == ')'))
            {
                bfr[1] = '\0';
                res = strdup(bfr);
                bptr = bfr;
                break;
            }
        if (bkflg) ch =')',bkflg = 0;
        else if ((ch = fgetc(input)) == EOF) break;
        if (ch == '"')
        {
            string temp = "";
            while((ch = fgetc(input)) !='"')
            {
                if ((char)ch == '\\')
                {
                    ch = fgetc(input);
                    int iter = zy_mp.count((char) ch);
                    if (iter == 0)throw NOChar();
                    ch = zy_mp[(char) ch];
                }
                temp += (char)ch;

            }
            stringstream ss;
            ss << '"' << temp << '"';
            temp = ss.str();
            return temp;
        }
        if (ch == '#')
        {
            string temp = "#";
            if(!isspace((ch = fgetc(input)))&& (char) ch != ')' )
            {

                temp += (char) ch;
                if ((char) ch =='t' || (char) ch == 'f') return temp;
                ch = fgetc(input);
                if (char (ch) == ')') return temp+')';
                do
                {
                    temp += (char) ch;
                }while(!isspace((ch = fgetc(input)))&& (char) ch != ')');
                if (isspace(ch)) return temp;
                if ((char) ch == ')') bkflg = 1; return temp;

            }
        }

        switch (ch)
        {
            case '(':
            case ')':
                if (bptr > bfr)
                {
                    *bptr = '\0';
                    res = strdup(bfr);
                }
                *bfr = (char)ch;
                bptr = bfr + 1;
                break;
            default:
                if (isspace(ch))
                {
                    if (bptr > bfr)
                    {
                        *bptr = '\0';
                        res = strdup(bfr);
                    }
                    bptr = bfr;
                }
                else
                    *bptr++ = (char)ch;
        }
    }
    if (res == NULL) throw TkExp();
    string tmp(res);
    return tmp;
}

Num *call_calc()
{
    static int layer = 0;
    static bool logic_done = 1;
    string tk0 ;
    int cnt = 0;
    while (logic_done == 0)
    {
        Num* tmp;
        do{
            tk0 = next_token();
            try{
            tmp = Char::from_str(tk0);
            if (!tmp) tmp = Int::from_str(tk0);
            if (!tmp) tmp = Rational::from_str(tk0);
            if (!tmp) tmp = Cmplx :: from_str(tk0);
            }
            catch(DtrNOzero) {throw DtrNOzero();}
            catch(NOChar) {throw NOChar();}
            catch (...) {}

        }
        while(tk0 != "(" && tk0 != ")");
        if (tk0 == "(") cnt--;
        if (tk0 == ")") cnt++;
        if (cnt == 1) break;
    }
    if (logic_done)tk0 = next_token();
    //low_down(tk0);
   // if(tk0 == "") throw TkExp();

    Num *res;
    if (tk0 == "(")
    {
        layer++;
        string tk1 = next_token();
        low_down(tk1);
        Opt *opt;
        Numlks *numlk = new Numlks (NULL, NULL), *endd = numlk;
        Num *val;
        int _logic = 0;
        if (tk1 == "" ) throw NOOp();
        if (tk1 == "+") opt = new Add ();
        else if (tk1 == "-") opt = new Sub();
        else if (tk1 == "*") opt = new Mul();
        else if (tk1 == "/") opt = new Div();
        else if (tk1 == "=") opt = new Equal();
        else if (tk1 == "<=") opt = new Less();
        else if (tk1 == ">=") opt = new Larger();
        else if (tk1 == "<") opt = new SLess ();
        else if (tk1 == ">") opt = new SLarger();
        else if (tk1 == "zero?") opt = new IsZero();
        else if (tk1 == "positive?") opt = new IsPositive();
        else if (tk1 == "negative?") opt = new IsNegative();
        else if (tk1 == "odd?") opt = new IsOdd();
        else if (tk1 == "even?") opt = new IsEven();
        else if (tk1 == "sin") opt = new Sin();
        else if (tk1 == "cos") opt = new Cos();
        else if (tk1 == "tan") opt = new Tan();
        else if (tk1 == "log") opt = new Log();
        else if (tk1 == "asin") opt = new Asin();
        else if (tk1 == "acos") opt = new Acos();
        else if (tk1 == "atan") opt = new Atan();
        else if (tk1 == "sqrt") opt = new Sqrt();
        else if (tk1 == "exp") opt = new Exp();
        else if (tk1 == "max") opt = new Max();
        else if (tk1 == "min") opt = new Min();
        else if (tk1 == "abs") opt = new Abs();
        else if (tk1 == "magnitude") opt = new Mgntd();
        else if (tk1 == "make-rectangular") opt = new MkRt();
        else if (tk1 == "make-polar") opt = new MkPl();
        else if (tk1 == "real-part") opt = new FindR();
        else if (tk1 == "imag-part") opt = new FindI();
        else if (tk1 == "angle") opt = new FindA();
        else if (tk1 == "equal?") opt = new IsEqual();
        else if (tk1 == "number?") opt = new IsNum();
        else if (tk1 == "integer?") opt = new IsInteger();
        else if (tk1 == "real?") opt = new IsReal();
        else if (tk1 == "rational?") opt = new IsRational();
        else if (tk1 == "exact?") opt = new IsExact();
        else if (tk1 == "inexact?") opt = new IsInexact();
        else if (tk1 == "complex?") opt = new IsComplex();
        else if (tk1 == "quotient") opt = new Quotient();
        else if (tk1 == "remainder") opt = new Remainder();
        ///==unsure!!!
        else if (tk1 == "modulo") opt = new Modulo();
        ///
        else if (tk1 == "gcd") opt = new Gcd();
        else if (tk1 == "lcm") opt = new Lcm();
        else if (tk1 == "numerator") opt = new Numerator();
        else if (tk1 == "denominator") opt = new Denominator();
        else if (tk1 == "floor") opt = new Floor();
        else if (tk1 == "ceiling") opt = new Ceiling();
        else if (tk1 == "round") opt = new Round();
        else if (tk1 == "truncate") opt = new Truncate();
        else if (tk1 == "expt") opt = new Expt();
        else if (tk1 == "newline") opt = new NewL();
        else if (tk1 == "exact->inexact") opt = new Ex2un();
        else if (tk1 == "inexact->exact") opt = new Ex2ex();
        else if (tk1 == "number->string") opt = new Num2str();
        else if (tk1 == "string->number") opt = new Str2num();
        else if (tk1 == "boolean?") opt = new Isbool();
        else if (tk1 == "not") opt = new Anti();
        else if (tk1 == "string?") opt = new IsString();
        else if (tk1 == "make-string") opt = new Mkstr();
        else if (tk1 == "string-length") opt = new Strlen();
        else if (tk1 == "string-ref") opt = new StrRf();
        else if (tk1 == "string=?") opt = new SIsequal();
        else if (tk1 == "string-ci=?") opt = new ScIsequal();
        else if (tk1 == "string<?") opt = new SIsSless();
        else if (tk1 == "string>?") opt = new SIsSlarger();
        else if (tk1 == "string<=?") opt = new SIsless();
        else if (tk1 == "string>=?") opt = new SIslarger();
        else if (tk1 == "string-ci<?") opt = new ScIsSless();
        else if (tk1 == "string-ci>?") opt = new ScIsSlarger();
        else if (tk1 == "string-ci<=?") opt = new ScIsless();
        else if (tk1 == "string-ci>=?") opt = new ScIslarger();
        else if (tk1 == "substring") opt = new Substr();
        else if (tk1 == "string-append") opt = new StrApp();
        else if (tk1 == "string-copy") opt = new StrCpy();
        else if (tk1 == "display") opt = new Display();
        else if (tk1 == "char?") opt = new IsChar();
        else if (tk1 == "char=?") opt = new CIsequal();
        else if (tk1 == "char-ci=?") opt = new CcIsequal();
        else if (tk1 == "char<?") opt = new CIsSless();
        else if (tk1 == "char>?") opt = new CIsSlarger();
        else if (tk1 == "char<=?") opt = new CIsless();
        else if (tk1 == "char>=?") opt = new CIslarger();
        else if (tk1 == "char-ci<?") opt = new CcIsSless();
        else if (tk1 == "char-ci>?") opt = new CcIsSlarger();
        else if (tk1 == "char-ci<=?") opt = new CcIsless();
        else if (tk1 == "char-ci>=?") opt = new CcIslarger();
        else if (tk1 == "char-alphabetic?") opt = new CIsAphb();
        else if (tk1 == "char-numeric?") opt = new CIsNum();
        else if (tk1 == "char-whitespace?") opt = new CIsWs();
        else if (tk1 == "char-upper-case?") opt = new CIsUp();
        else if (tk1 == "char-lower-case?") opt = new CIsDwn();
        else if (tk1 == "char->integer") opt = new C2Int();
        else if (tk1 == "integer->char") opt = new Int2C();
        else if (tk1 == "char-upcase") opt = new Cup();
        else if (tk1 == "char-downcase") opt = new Cdown();
        else if (tk1 == "string") opt = new MkStr();
        else if (tk1 == "procedure?") opt = new IsProd();

        else if (tk1 == "and") _logic = 1;
        else if (tk1 == "or") _logic = 2;
        else if (tk1 == "if") _logic = 3;

        else throw NOaOp();

        if (_logic == 0)
        {
            while (val = call_calc())
            {
                endd->next = new Numlks(val, NULL);
                endd = endd->next;
            }
            res = opt->calc(numlk->next);
            for (Numlks *tmp; numlk; )
            {
                tmp = numlk->next;
                delete numlk;
                numlk = tmp;
            }
        }
        else if ( _logic == 1)
        {
            int cnt = 0;
            while (val = call_calc())
            {
                ++cnt;
                res = val;
                if (val->tplvl == -1 && SCAST_B(val)->x == 0)
                {
                    logic_done = 0;
                }
            }
            if (val == NULL && cnt == 0) {res = new Bool(1);return res;}
        }
        else if (_logic == 2)
        {
            int cnt = 0;
            while (val = call_calc())
            {
                ++cnt;
                if (val->tplvl == -1 && SCAST_B(val)->x == 0)
                {
                    res = val;
                }
                else res = val, logic_done = 0;
            }
            if (val == NULL && cnt == 0) {res = new Bool(0);return res;}
        }
        else if (_logic == 3)
        {
            val = call_calc();
            bool flgg;
            if (val->tplvl != -1 || SCAST_B(val)->x != 0) flgg = 1;
            else flgg = 0;
            Num* yes,*noo;
            if (flgg)
            {
                yes = call_calc();
                string tmp0;
                int cnt = 0;
                bool tiqian = 0;
                int  fir_cnt = 0;
                while(1){
                //do{
                    ++fir_cnt;
                    tmp0 = next_token();
                    if (tmp0 == ")" && fir_cnt == 1) {tiqian = 1;break;}
                    //cout << tmp0 <<endl;
                    try{
                    noo = Char::from_str(tmp0);
                    if (!noo) noo = Int::from_str(tmp0);
                    if (!noo) noo = Rational::from_str(tmp0);
                    if (!noo) noo = Cmplx :: from_str(tmp0);
                    }
                    catch(DtrNOzero) {throw DtrNOzero();}
                    catch(NOChar) {throw NOChar();}
                    catch (...) {}
                    //}
                    //while(tmp0 != "(" && tmp0 != ")");
                    if (tmp0 == "(") cnt--;
                    if (tmp0 == ")") cnt++;
                    if (cnt == 0) break;
                }

                res = yes;
                if (!tiqian)while (call_calc());
                return res;

            }
            else
            {
                string tmp0;
                int cnt = 0;
                while(1){
               // do{
                    tmp0 = next_token();
                    //cout << tmp0 <<endl;

                    try{
                    yes = Char::from_str(tmp0);
                    if (!yes) yes = Int::from_str(tmp0);
                    if (!yes) yes = Rational::from_str(tmp0);
                    if (!yes) yes = Cmplx :: from_str(tmp0);
                    }
                    catch(DtrNOzero) {throw DtrNOzero();}
                    catch(NOChar) {throw NOChar();}
                    catch (...) {}
                    //}
                   // while(tmp0 != "(" && tmp0 != ")");
                    if (tmp0 == "(") cnt--;
                    if (tmp0 == ")") cnt++;
                    if (cnt == 0) break;
                }

                noo = call_calc();
                res = noo;
                if (!noo) res = new Bool(0);
                if(noo)
                    while (call_calc());
                //res = new Bool(0);
                return res;
            }

        }

    }
    else if (tk0 == ")")
    {
        layer--;
        if (logic_done == 0) logic_done = 1;
        if (layer < 0) throw NOfitBkt();
        return NULL;
    }
    else if (tk0 == "exit") {exit(0);}
    else
    {
        if (tk0 == "Pi") res = new Float (3.1415926),res->exact2un(),res->Q2CrQ();
        else if (tk0 == "E") res = new Float (2.78182818),res->exact2un(),res->Q2CrQ();
        else if (tk0 =="ans") res = lans;
        else{
        if (tk0[0] == '\"' && tk0[tk0.length()-1] == '\"') res = new String(tk0.substr(1,tk0.length()-2));
        else{
        if (tk0[0] == '#'&&tk0.length() == 2) res = Bool::from_str(tk0);
        else{
        res = Char::from_str(tk0);
        if (!res) res = Prod::from_str(tk0);
        low_down(tk0);
        if (!res) res = Int::from_str(tk0);
        if (!res) res = Rational::from_str(tk0);
        if (!res) res = Cmplx :: from_str(tk0);
        ///unsure!!!
        if (!res) res = Float::from_str(tk0);
        ///
        if (!res) throw NaN();
        }
        }
        }
    }
    return res;
}

int main(int argc, char *argv[])
{
    ios::sync_with_stdio(0);
    // std::cout << "hello" << std::endl;
    for (int file_num=1; file_num<=argc; ++file_num) {
        if (freopen(argv[file_num], "r", stdin)==NULL) {
           // throw NOFile();
        }

    //cerr << Procedure.size() <<endl;
    for (Num *res;;)
    {
        try
        {
            if(!(res = call_calc())) throw InvldExp();
            lans = res;
            //if(res->dsply != 1)  res->print();
            //cerr <<endl <<"=======" << res->tplvl <<"======"<<endl;
        }
        catch(NOlow2high){cerr << "Error: Cannot convert lower type to higher type" <<endl;return 1;}
        catch(NOtype) {cerr << "Error: Type has not been implemented" << endl;return 1;}
        catch(DtrNOzero){cerr << "Error: Denominator cannot not be zero" << endl;return 1;}
        catch(NOdivzero){cerr << "Error: Cannot divide by zero" << endl;return 1;}
        catch(Ag2many){cerr << "Error: Too many arguments" << endl;return 1;}
        catch(Ag2less){cerr << "Error: Too less arguments" << endl;return 1;}
        catch(FirW){cerr << "Error: First argument type is wrong" <<endl;return 1;}
        catch(SecW){cerr << "Error: Second argument type is wrong" <<endl;return 1;}
        catch(SecNOzero){cerr << "Error: Second argument cannot be zero" <<endl;return 1;}
        catch(NOallInt){cerr << "Error: Not all int" <<endl;return 1;}
        catch(NOCrQ2Q){cerr << "Error: Cannot convert CrQ to Q" <<endl;return 1;}
        catch(NOallZero){cerr << "Error: Cannot all be zero" <<endl;return 1;}
        catch(NOCp2R){cerr << "Error: Complex cannot be compared" <<endl;return 1;}
        catch(TkExp){break;}
        catch(NOOp){cerr << "Error: Procedure requires at least one operator" <<endl;return 1;}
        catch(NOaOp){cerr << "Error: Procedure operator not found" <<endl;return 1;}
        catch(NOfitBkt){cerr << "Error: Brackets cannot fit" <<endl;return 1;}
        catch(NaN){cerr << "Error: Not acceptable" <<endl;return 1;}
        catch(InvldExp){cerr << "Error: Invalid expression" <<endl;return 1;}
        catch(OnlyInt){cerr << "Error: Only int accepted" <<endl;return 1;}
        catch(OnlyE_NN_Int){cerr << "Error: Only exact non-negative int accepted" <<endl;return 1;}
        catch(LkAg) {cerr << "Error: Lack of Argument" <<endl;return 1;}
        catch(TypeW) {cerr << "Error: Wrong type of argument" <<endl;return 1;}
        catch(IdxW) {cerr << "Error: Index wrong" <<endl;return 1;}
        catch(NOChar) {cerr << "Error: Can't convert to char" <<endl;return 1;}
        catch(NOFile) {cerr << "Error: File can't be opened" <<endl;return 1;}
        catch(...){cerr << "Error: Unknown error" <<endl;return 1;}
    }
    }
    return 0;
}


