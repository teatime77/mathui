from syntax import Num, Name, Variable, ClassDef, Statement, Term, Obj, Index, Tuple2, List2, Subscript, Slice, ExtSlice
from syntax import Assign, BinOp, Call, Expr, Body, Compare, BoolOp, NameConstant

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
%s
</body>
</html>
"""

math_op_dic = {
     '*'  :'&middot;', 
     '=='  :'=', 
     '!=' :'&ne;' , 
     '<'  :'&lt;', 
     '<=' :'&le;', 
     '>'  :'&gt;' , 
     '>=' :'&ge;' 
}

def ml(o):
    if o is None:
        return ''
    else:
        return o.mathml()

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

def make_mathml(obj):
    return '<math>\n%s\n</math>\n' % obj.mathml()

def Obj_mathml(self:Obj):
    assert False
    return None

def Num_mathml(self:Num):
    return '<mn>%s</mn>' % self.n

def NameConstant_mathml(self: NameConstant):
    if self.value is None:
        return ''
    else:
        return '<mi>%s</mi>' % self.value

def Name_mathml(self:Name):
    if self.id.endswith('_'):
        name = toMathMLName(self.id[:-1])
        return "<mrow><mi>%s</mi><mo>'</mo></mrow>" % name
    else:
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

def BoolOp_mathml(self: BoolOp):
    s =  ('<mo> %s </mo>' % math_op(self.op)).join(x.mathml() for x in self.values)
    if self.with_parenthesis:
        return '<mfenced separators="">%s</mfenced>' % s
    else:
        return '<mrow>%s</mrow>' % s



def Variable_mathml(self: Variable):
    return '<mrow><mi>%s</mi><mo>&isin;</mo>%s</mrow>' % (toMathMLName(self.name), self.type.mathML())

def ClassDef_mathml(self: ClassDef):
    return '<mi>%s</mi>' % self.name

def Statement_mathml(self: Statement):
    assert False
    return None

def Expr_mathml(self: Expr):
    return self.value.mathml()

def Body_mathml(self: Body):
    return '\n'.join(x.mathml() for x in self.statements)

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

def Tuple2_mathml(self: Tuple2):
    return '<mfenced>%s</mfenced>' % ''.join(x.mathml() for x in self.elts)

def List2_mathml(self: List2):
    return '<mfenced open="[" close="]">%s</mfenced>' % ''.join(x.mathml() for x in self.elts)

def Index_mathml(self: Index):
    if isinstance(self.value, Tuple2):
        return '<mrow>%s</mrow>' % '<mo>,</mo>'.join( x.mathml() for x in self.value.elts )
    else:
        return self.value.mathml()

def Slice_mathml(self: Slice):

    return '<mfenced>%s</mfenced>' % ''.join(x.mathml() for x in self.dims)

def ExtSlice_mathml(self: ExtSlice):
    return '<mfenced>%s</mfenced>' % ''.join(x.mathml() for x in self.dims)

def Subscript_mathml(self: Subscript):
    value = self.value.mathml()
    if isinstance(self.slice, Slice):
        assert self.slice.upper is not None
        if self.slice.lower is None:
            return '<msup>%s %s</msup>' % (value, ml(self.slice.upper))
        else:
            return '<msubsup>%s %s %s</msubsup>' % (value, ml(self.slice.lower), ml(self.slice.upper))

    elif isinstance(self.slice, Index) and isinstance(self.slice.value, Tuple2) and len(self.slice.value.elts) == 2:
        elts = self.slice.value.elts
        return '<msubsup>%s %s %s</msubsup>' % (value, ml(elts[0]), ml(elts[1]))
    else:
        slice = self.slice.mathml()
        return '<msub>%s%s</msub>' % (value, slice)

def is_none(o):
    if isinstance(o, NameConstant) and o.value is None:
        return True
    else:
        return False
    
def Call_mathml(self: Call):
    formats = {
        # 'sum'     : '<mrow> <munderover><mo>&Sum;</mo> <mrow>%s<mo>=</mo>%s</mrow> %s </munderover> %s </mrow>',
        'sup'     : '<msup>%s %s</msup>',
        'subsup'  : '<msubsup>%s %s %s</msubsup>',
        'log'     : '<mrow><mi>log</mi>%s</mrow>',
        'sqrt'    : '<msqrt>%s</msqrt>',
        'integral': '<mrow> <munderover><mo>&#x222B;</mo> %s %s </munderover> %s <mi>d</mi> %s </mrow>',
        'lim'     : '<mrow> <munder><mo>lim</mo> <mrow>%s<mo>&rarr;</mo>%s</mrow></munder> %s </mrow>',
        'diff'     : '<mfrac><mrow><mi>d</mi>%s</mrow> <mrow><mi>d</mi>%s</mrow></mfrac>',
        'pdiff'     : '<mfrac><mrow><mi>&part;</mi>%s</mrow> <mrow><mi>&part;</mi>%s</mrow></mfrac>',
        'norm'    : '<mfenced open="||" close="||"> %s </mfenced>',
        'prob'    : '<mrow> <mi>P</mi> <mfenced separators=""> %s <mo>|</mo> %s </mfenced></mrow>',
    }

    args = tuple(ml(x) for x in self.args)
    if isinstance(self.func, Name):
        func_name = self.func.get_name()

        if func_name in formats:
            return formats[func_name] % args
        elif func_name == 'sum':
            if is_none(self.args[1]):
                fmt = '<mrow> <munderover><mo>&Sum;</mo> <mrow>%s %s</mrow> %s </munderover> %s </mrow>'
            else:
                fmt = '<mrow> <munderover><mo>&Sum;</mo> <mrow>%s<mo>=</mo>%s</mrow> %s </munderover> %s </mrow>'
            return fmt % args
        else:
            return '<mrow>%s<mfenced open="(" close=")">%s</mfenced></mrow>' % (self.func.mathml(), ''.join(args))
    else:
        # return '<mrow><mfenced> %s </mfenced><mfenced open="(" close=")">%s</mfenced></mrow>' % (self.func.mathml(), ''.join(args))
        return '<mrow>%s<mfenced open="(" close=")">%s</mfenced></mrow>' % (self.func.mathml(), ''.join(args))

def Compare_mathml(self: Compare):
        s = ' '.join('<mo>%s</mo> %s' % (math_op_dic[op], t.mathml()) for op, t in zip(self.ops, self.comparators))
        return '<mrow>%s %s</mrow>' % (self.left.mathml(), s )

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