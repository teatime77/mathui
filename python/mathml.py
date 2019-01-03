from syntax import Num, Name, Variable, ClassDef, Statement, Term, Obj, Index, Tuple_, Subscript
from syntax import Assign, BinOp, Call, Expr

html_str = """<!DOCTYPE html>
<html lang="ja">
<head>
    <meta charset="utf-8">
    <title>MathML - 数式処理</title>

    <script type="text/x-mathjax-config">
        MathJax.Hub.Config({
            extensions: [ "tex2jax.js", "mml2jax.js" ],
            jax: ["input/TeX", "input/MathML", "output/SVG"],
        })
    </script>
    
    <script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=default"></script>
</head>
<body>
<math>
%s
</math>
</body>
</html>
"""

math_op_dic = {
     '*'  :'&middot;', 
     '!=' :'&ne;' , 
     '<'  :'&lt;', 
     '<=' :'&le;', 
     '>'  :'&gt;' , 
     '>=' :'&ge;' 
}

def toMathMLName(name : str):
    v = [ 
        'alpha', 'beta', 'gamma', 'delta', 'epsilon', 'zeta', 'eta', 'theta', 
        'iota', 'kappa', 'lambda', 'mu', 'nu', 'xi', 'pi', 'rho', 
        'sigma', 'tau', 'phi', 'chi', 'psi', 'omega'
    ]

    k = name.find('__')
    if k != -1:
        name1 = name[:k]

    else:
        name1 = name

    name1_low = name1[0].lower() + name1[1:]
    if name1_low in v:

        name1 = '&' + name1 + ';'

    if k != -1:

        mark = name[k + 2:]
        return '<mover> <mi>%s</mi> <mo>&%s;</mo> </mover>' % (name1, mark)

    else:
        return name1



def math_op(op):
    if op in math_op_dic:
        return math_op_dic[op]
    else:
        return op

def make_mathml_html(obj):
    return html_str % obj.mathml()

def Obj_mathml(self:Obj):
    assert False
    return None

def Num_mathml(self:Num):
    return '<mn>%s</mn>' % self.n


def Name_mathml(self:Name):
    name = toMathMLName(self.id)
    return '<mi>%s</mi>' % name

def Name_mathml2(self: Name):
    name = toMathMLName(self.id)

    mml = '<mi>%s</mi>' % name

    return self.mmlMetaId( self.mulVal(mml) )

def BinOp_mathml(self: BinOp):

    left = self.left.mathml()
    right = self.right.mathml()
    if self.op == '/':
        s = '<mfrac>%s %s</mfrac>' % (left, right)

    elif self.op == '**':
        # s = '<msup><mfenced>%s</mfenced> %s</msup>' % (left, right)
        s = '<msup>%s %s</msup>' % (left, right)

    else:
        s = '<mrow>%s<mo>%s</mo>%s</mrow>' % (left, math_op(self.op), right)
    if self.with_parenthesis:
        return '<mfenced open="(" close=")">%s</mfenced>' % s
    else:
        return s

def Variable_mathml(self: Variable):
    return '<mrow><mi>%s</mi><mo>&isin;</mo>%s</mrow>' % (toMathMLName(self.name), self.type.mathML())

def ClassDef_mathml(self: ClassDef):
    return '<mi>%s</mi>' % self.name

def Statement_mathml(self: Statement):
    assert False
    return None

def Expr_mathml(self: Expr):
    return self.value.mathml()

def list_mathml(tpl):
    if len(tpl) == 1:
        return tpl[0].mathml()
    else:
        return '<mfenced> %s </mfenced>' % ''.join( x.mathml() for x in tpl )

def Assign_mathml(self: Assign):
    return '<mrow>%s<mo>=</mo>%s</mrow>' % (list_mathml(self.targets), self.value.mathml())

def Term_mathml(self: Term):
    assert False
    return None

# def Num_mathml():
#     return '<mn id='%s'>%s</mn>' % (self.id, '' + self.value)

def Index_mathml(self: Index):
    if isinstance(self.value, Tuple_):
        return '<mrow>%s</mrow>' % '<mo>,</mo>'.join( x.mathml() for x in self.value.elts )
    else:
        return self.value.mathml()

def Subscript_mathml(self: Subscript):
    return '<msub>%s%s</msub>' % (self.value.mathml(), self.slice.mathml())

def Call_mathml(self: Call):
    args = tuple(x.mathml() for x in self.args)
    if isinstance(self.func, Name):
        func_name = self.func.get_name()
        if func_name == 'sum':
            return '<mrow> <munderover><mo>&Sum;</mo> <mrow>%s<mo>=</mo>%s</mrow> %s </munderover> %s </mrow>' % args

        elif func_name == 'sqrt':
            return '<msqrt>%s</msqrt>' % self.args[0].mathml()

        elif func_name == 'integral':
            return '<mrow> <munderover><mo>&#x222B;</mo> %s %s </munderover> %s <mi>d</mi> %s </mrow>' % args
        elif func_name == 'lim':
            return '<mrow> <munder><mo>lim</mo> <mrow>%s<mo>&rarr;</mo>%s</mrow></munder> %s </mrow>' % args
        elif func_name == 'dif':
            return '<mfrac><mrow><mi>d</mi>%s</mrow> <mrow><mi>d</mi>%s</mrow></mfrac>' % args
        elif func_name == 'norm':
            # return '<mfenced open="||" close="||"> %s </mfenced>' % args[0]
            return '<mfenced open="||" close="||"> %s </mfenced>' % args
        else:
            return '<mrow>%s<mfenced open="(" close=")">%s</mfenced></mrow>' % (self.func.mathml(), ''.join(args))
    else:
        return '<mrow><mfenced> %s </mfenced><mfenced open="(" close=")">%s</mfenced></mrow>' % (self.func.mathml(), ''.join(args))

"""
mathMLSub2(: 
    var texs = self.args.map(x => x.mathML())

    if self.functionApp instanceof Reference: 

        switch(self.functionApp.name: 
        case '+':
            var s = ''
            for(let [idx, arg] of self.args.entries(): 
                if 0 < idx && 0 <= arg.value: 
                    s += '<mo>+</mo>'
            
                s += texs[idx]
        
            return '<mrow>' + s + '</mrow>'



VariableDeclaration mathML():
    if self.variables.length == 1: 
        return self.variables[0].mathML()

    else:

        return '<mrow id='%s'>%s</mrow>' % self.id, self.variables.map(x => x.mathML()).join('<mo>,</mo>')

"""