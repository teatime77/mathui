import ast
import codecs
import numpy as np
from typing import List, Tuple, Dict
import collections


if ((1 + 2) * 3 == 0 or 1 == 2) and 2 == 3:
    pass
a : Dict[str, int]
b : List[int]

opr_dic = {
    ast.Eq : '==',
    ast.Lt  : '<',
    ast.LtE : '<=',
    ast.Add : '+',
    ast.Sub : '-',
    ast.USub : '-',
    ast.Mult: '*',
    ast.Div : '/',
    ast.Mod : '%',
    ast.Pow : '**',
    ast.Not : 'not',
    ast.Is : 'is',
    ast.IsNot : 'is not',
    ast.NotEq : '!=',
    ast.Or : 'or',
    ast.And: 'and',
    ast.In : 'in'
}

    

x = 1
x: float = 1.2
x = (1,2)
y = x[1]

def sub(a:int, b: int, e: List[Tuple[int,float]]) -> int:
    c = a + b
    return c

def find(search_iteration):
    """リスト内で条件に合う要素を返す。
    """
    try:
        return next(search_iteration)
    except StopIteration:
        return None

def Hi(self, msg):
    print(msg)

class Obj:
    __slots__ = [ 'parent' ]

    def __init__(self):
        pass

    def get_name(self):
        if isinstance(self, Name):
            return self.id
        else:
            return None

    def resolve_name(self, vars):
        pass

class Type(Obj):
    pass

class ArrayType(Type):
    def __init__(self, element_type, shape: List[int] = None):
        self.element_type = element_type
        self.shape = shape

    def __str__(self):
        return 'List[%s]' % self.element_type

class TupleType(Type):
    def __init__(self, element_types):
        self.element_types = element_types
        for x in element_types:
            if isinstance(x, Obj):
                x.parent = self

    def __str__(self):
        return 'Tuple[%s]' % ', '.join(str(x) for x in self.element_types)

class DictType(TupleType):
    def __init__(self, element_types):
        super().__init__(element_types)

    def __str__(self):
        return 'Dict[%s]' % ', '.join(str(x) for x in self.element_types)

def to_type(trm):    
    if isinstance(trm, Subscript):
        assert isinstance(trm.slice, Index)
        name = trm.value.get_name()
        if name == 'Dict' or name == 'Tuple':
            assert isinstance(trm.slice.value, Tuple_)
            element_types = [ to_type(x) for x in trm.slice.value.elts ]

            if name == 'Dict':
                return DictType(element_types)
            else:
                return TupleType(element_types)

        elif name == 'List':
            element_type = to_type(trm.slice.value)
            return ArrayType(element_type)

    return trm

class Term(Obj):
    __slots__ = [ 'type' ]

class Tuple_(Term):
    __slots__ = [ 'elts' ]

    def __init__(self, expr):
        self.elts = [ term(self, x) for x in expr.elts ]

    def __str__(self):
        return '(' + ', '.join(str(x) for x in self.elts) + ')'

class List_(Term):
    __slots__ = [ 'elts' ]

    def __init__(self, expr):
        self.elts = [ term(self, x) for x in expr.elts ]

    def __str__(self):
        return '[' + ', '.join(str(x) for x in self.elts) + ']'

class Dict_(Term):
    __slots__ = [ 'keys', 'values' ]

    def __init__(self, expr):
        self.keys = [ term(self, x) for x in expr.keys ]
        self.values = [ term(self, x) for x in expr.values ]

    def __str__(self):
        return '{' + ', '.join('%s : %s' % (k, v) for k, v in zip(self.keys, self.values)  ) + '}'


class Index(Term):
    __slots__ = [ 'value' ]

    def __init__(self, expr):
        self.value = term(self, expr.value)

    def __str__(self):
        if isinstance(self.value, Tuple_):
            return ', '.join(str(x) for x in self.value.elts)
        else:
            return str(self.value)

class Slice(Obj):
    __slots__ = [ 'lower', 'upper', 'step' ]

    def __init__(self, expr):
        self.lower = term(self, expr.lower)
        self.upper = term(self, expr.upper)
        self.step = term(self, expr.step)

    def __str__(self):
        if self.lower is None:
            s1 = ''
        else:
            s1 = str(self.lower)

        if self.upper is None:
            s2 = ''
        else:
            s2 = str(self.upper)

        return '%s:%s' % (s1, s2)


class Subscript(Term):
    __slots__ = [ 'slice', 'value' ]

    def __init__(self, expr):
        self.slice = term(self, expr.slice)
        self.value = term(self, expr.value)


    def __str__(self):
        return str(self.value) + '[' + str(self.slice) + ']'

# def find_var(vars, name):

class Name(Term):
    __slots__ = [ 'id', 'var', 'mathml' ]

    def __init__(self, expr):
        self.id = expr.id
        self.var = None

    def __str__(self):
        return self.id

    def resolve_name(self, vars):
        for v in vars:
            var = find(x for x in v if x.name == self.id)
            if var is not None:
                self.var = var
                break


class NameConstant(Term):
    __slots__ = [ 'value' ]

    def __init__(self, expr):
        self.value = term(self, expr.value)

    def __str__(self):
        return str(self.value)

class Str(Term):
    __slots__ = [ 's' ]

    def __init__(self, expr):
        self.s = expr.s

    def __str__(self):
        
        return repr(self.s)

class Num(Term):
    __slots__ = [ 'n' ]

    def __init__(self, expr):
        self.n = expr.n

    def __str__(self):
        return str(self.n)


class Bool(Term):
    __slots__ = [ 'b' ]

    def __init__(self, expr):
        self.b = expr

    def __str__(self):
        return str(self.b)



class Attribute(Term):
    __slots__ = [ 'attr', 'value', 'var' ]

    def __init__(self, expr):
        self.attr = expr.attr
        self.value = term(self, expr.value)
        self.var = None

    def __str__(self):
        return str(self.value) + '.' + self.attr

class ListComp(Term):
    __slots__ = [ 'elt', 'generators' ]

    def __init__(self, expr):
        self.elt = term(self, expr.elt)
        self.generators = [ Comprehension(self, x) for x in expr.generators ]

    def __str__(self):
        return '[%s %s]' % (self.elt, ' '.join(str(x) for x in self.generators) )

class Compare(Term):
    __slots__ = [ 'left', 'ops', 'comparators' ]

    def __init__(self, expr):
        self.left = term(self, expr.left)
        self.ops = [opr_dic[type(x)] for x in expr.ops]
        self.comparators = [ term(self, x) for x in expr.comparators ]

    def __str__(self):
        s = ' '.join('%s %s' % (op, t) for op, t in zip(self.ops, self.comparators))
        return '%s %s' % (self.left, s )

class UnaryOp(Term):
    __slots__ = [ 'op', 'operand' ]

    def __init__(self, expr):
        self.op   = opr_dic[type(expr.op)]
        self.operand = term(self, expr.operand)

    def __str__(self):
        return '%s %s' % (self.op, self.operand)

class BinOp(Term):
    __slots__ = [ 'left', 'op', 'right', 'with_parenthesis' ]

    def __init__(self, expr):
        self.left = term(self, expr.left)
        self.op   = opr_dic[type(expr.op)]
        self.right = term(self, expr.right)
        self.with_parenthesis = None

    def __str__(self):
        s = '%s %s %s' % (self.left, self.op, self.right)
        if self.with_parenthesis:
            return '(' + s + ')'
        else:
            return s

    def getPrecedence(self):
        if self.op in [ '**' ]:
            return 1
        elif self.op in [ '*', '/' ]:
            return 2
        elif self.op in [ '+', '-' ]:
            return 3
        else:
            assert False
            return None

class BoolOp(Term):
    __slots__ = [ 'op', 'values', 'with_parenthesis' ]

    def __init__(self, expr):
        self.op   = opr_dic[type(expr.op)]
        self.values = [ term(self, x) for x in expr.values ]
        self.with_parenthesis = None

    def __str__(self):
        s =  (' %s ' % self.op).join(str(x) for x in self.values)
        if self.with_parenthesis:
            return '(' + s + ')'
        else:
            return s

    def getPrecedence(self):
        if self.op == 'not':
            return 1
        elif self.op == 'and':
            return 2
        elif self.op == 'or':
            return 3
        else:
            assert False

class Keyword(Obj):
    __slots__ = [ 'arg', 'value' ]
    def __init__(self, parent, kw: ast.keyword):
        self.parent = parent
        self.arg  = kw.arg
        self.value = term(self, kw.value)

class Call(Term):
    __slots__ = [ 'func', 'args', 'keywords' ]

    def __init__(self, expr: ast.Call):
        self.func = term(self, expr.func)
        self.args = [ term(self, x) for x in expr.args]
        self.keywords = collections.OrderedDict( (x.arg, term(self, x.value)) for x in expr.keywords)

    def __str__(self):
        return '%s(%s)' % (self.func, ', '.join(str(x) for x in self.args))

class Statement(Obj):
    pass

class Pass(Statement):
    def __str__(self):
        return 'pass\n'

class Continue(Statement):

    def __str__(self):
        return 'continue\n'

class Break(Statement):

    def __str__(self):
        return 'break\n'

class Assert(Statement):
    __slots__ = [ 'test', 'msg' ]

    def __init__(self, stmt):
        self.test = term(self, stmt.test)
        self.msg  = stmt.msg

    def __str__(self):
        return 'assert %s\n' % self.test

class Return(Statement):
    __slots__ = [ 'value' ]

    def __init__(self, expr):
        self.value = term(self, expr.value)

    def __str__(self):
        if self.value is None:
            return 'return\n'
        else:
            return 'return %s\n' % self.value

class Alias:
    __slots__ = [ 'name', 'asname' ]

    def __init__(self, alias: ast.alias):
        self.name = alias.name
        self.asname = alias.asname

    def __str__(self):
        if self.asname is None:
            return self.name
        else:
            return '%s as %s' % (self.name, self.asname)

class Import(Statement):
    __slots__ = [ 'names' ]

    def __init__(self, stmt):
        self.names = [ Alias(x) for x in stmt.names ]

    def __str__(self):
        return 'import %s\n' % ', '.join(str(x) for x in self.names)

class ImportFrom(Statement):
    __slots__ = [ 'module', 'names' ]

    def __init__(self, stmt: ast.ImportFrom):
        self.module = stmt.module
        self.names = [ Alias(x) for x in stmt.names ]

    def __str__(self):
        return 'from %s import %s\n' % (self.module, ', '.join(str(x) for x in self.names))

class Assign(Statement):
    __slots__ = [ 'targets', 'value' ]

    def __init__(self, stmt):
        self.targets = [ term(self, x) for x in stmt.targets ]
        self.value = term(self, stmt.value)

    def __str__(self):
        return '%s = %s\n' % ( ', '.join(str(x) for x in self.targets), self.value)

class AugAssign(Assign):
    __slots__ = [ 'targets', 'value', 'op' ]

    def __init__(self, stmt):
        self.targets = [ term(self, stmt.target) ]
        self.value = term(self, stmt.value)
        self.op   = opr_dic[type(stmt.op)]

    def __str__(self):
        return '%s %s= %s\n' % ( ', '.join(str(x) for x in self.targets), self.op, self.value)

class AnnAssign(Assign):
    __slots__ = [ 'targets', 'value', 'tp' ]

    def __init__(self, stmt):
        self.targets = [ term(self, stmt.target) ]
        self.value = term(self, stmt.value)
        self.tp = to_type( term(self, stmt.annotation) )

    def __str__(self):
        return '%s: %s = %s\n' % (self.targets[0], self.tp, self.value)

def target_vars(parent, target):
    if isinstance(target, ast.Name):
        return [ Variable(parent, target.id)  ]

    elif isinstance(target, ast.Tuple):
        for x in target.elts:
            assert isinstance(x, ast.Name)

        return [ Variable(parent, x.id) for x in target.elts ]

    else:
        assert False

class Comprehension:
    __slots__ = [ 'parent', 'iter', 'target', 'ifs' ]

    def __init__(self, parent, cmp):
        self.parent = parent
        self.iter = term(self, cmp.iter)
        self.target = target_vars(self, cmp.target)
        self.ifs = [ term(self, x) for x in cmp.ifs]

    def __str__(self):
        target = ', '.join(str(x) for x in self.target)
        header = 'for %s in %s' % (target, self.iter)
        ifs = ''.join( ' if %s' % x for x in self.ifs )
        return header + ifs
        

class GeneratorExp:
    __slots__ = [ 'parent', 'elt', 'generators' ]

    def __init__(self, expr):
        self.elt = term(self, expr.elt)
        self.generators = [ Comprehension(self, x) for x in expr.generators ]

    def __str__(self):
        return '%s %s' % (self.elt, ' '.join(str(x) for x in self.generators))

class Expr(Statement):
    __slots__ = [ 'value' ]

    def __init__(self, stmt):
        self.value = term(self, stmt.value)

    def __str__(self):
        return '%s\n' % self.value

class WithItem(Obj):
    __slots__ = [ 'parent', 'context_expr', 'optional_vars' ]

    def __init__(self, parent, item):
        self.parent = parent
        self.context_expr = term(self, item.context_expr)
        self.optional_vars = term(self, item.optional_vars)

    def __str__(self):
        return '%s as %s' % (self.context_expr, self.optional_vars)


class With(Statement):
    __slots__ = [ 'items', 'body' ]

    def __init__(self, stmt):
        self.items = [ WithItem(self, x) for x in stmt.items ]
        self.body = Body(self, stmt.body)

    def __str__(self):
        header = 'with %s:' % ', '.join(str(x) for x in self.items)
        return '%s\n%s' % (header, self.body.str_indent())

class If(Statement):
    __slots__ = [ 'prev', 'test', 'body', 'orelse' ]

    def __init__(self, stmt):
        self.prev = None
        self.test = term(self, stmt.test)
        self.body = Body(self, stmt.body)
        self.orelse = Body(self, stmt.orelse)
        if len(stmt.orelse) != 0:
            if isinstance(stmt.orelse[0], If):
                stmt.orelse[0].prev = self
            # print(', '.join(str(type(x)) for x in stmt.orelse))
            # print('')

    def __str__(self):
        return self.elif_str(True)

    def elif_str(self, is_if):
        if is_if:
            header = 'if %s:\n' % self.test
        else:
            header = 'elif %s:\n' % self.test

        header_body = header + self.body.str_indent()

        orelse = self.orelse.statements
        if len(orelse) == 0:
            return header_body
        elif len(orelse) == 1 and isinstance(orelse[0], If):
            return header_body + orelse[0].elif_str(False)
        else:
            return header_body + 'else:\n' + self.orelse.str_indent()


class For(Statement):
    __slots__ = [ 'target', 'iter', 'body' ]

    def __init__(self, stmt):

        self.target = target_vars(self, stmt.target)

        self.iter = term(self, stmt.iter)
        self.body = Body(self, stmt.body)

    def __str__(self):
        target = ', '.join(str(x) for x in self.target)
        header = 'for %s in %s:' % (target, self.iter)
        return '%s\n%s' % (header, self.body.str_indent())

class ExceptHandler(Statement):
    __slots__ = [ 'type', 'body' ]

    def __init__(self, parent, stmt):
        self.parent = parent
        self.body = Body(self, stmt.body)
        self.type = to_type(term(self, stmt.type))

    def __str__(self):
        return 'except %s:\n' % self.type + self.body.str_indent()

class Try(Statement):
    __slots__ = [ 'handlers', 'body' ]

    def __init__(self, stmt):
        self.body = Body(self, stmt.body)
        self.handlers = [ ExceptHandler(self, x) for x in stmt.handlers ]

    def __str__(self):
        body = 'try:\n' + self.body.str_indent()
        for x in self.handlers:
            body += str(x)

        return body
    

class Variable(Obj):
    __slots__ = [ 'parent', 'name', 'type' ]

    def __init__(self, parent, name):
        self.parent = parent
        self.name = name
        self.type = None

    def __str__(self):
        return self.name

class Arg(Variable):
    __slots__ = [ 'default' ]

    def __init__(self, parent, arg: ast.arg):
        super().__init__(parent, arg.arg)
        if arg.annotation is None:
            self.type = None
        else:
            self.type = to_type( term(self, arg.annotation) )
        self.default = None

    def __str__(self):
        if self.default is None:
            default = ''
        else:
            default = ' = %s' % self.default

        if self.type is None:
            return self.name + default
        elif isinstance(self.type, ClassDef):
            return '%s: %s%s' % (self.name, self.type.name, default)
        else:
            return '%s: %s%s' % (self.name, self.type, default)

class Field(Variable):

    def __init__(self, parent, name):
        super().__init__(parent, name)
        self.type = None
        # attr = asn.targets[0]
        # self.name = attr.attr
        # attr.attr.var = self

def to_stmt(parent, stmt):
    if isinstance(stmt, ast.Import):
        stmt2 = Import(stmt)
    elif isinstance(stmt, ast.ImportFrom):
        stmt2 = ImportFrom(stmt)
    elif isinstance(stmt, ast.Assign):
        stmt2 = Assign(stmt)
    elif isinstance(stmt, ast.AugAssign):
        stmt2 = AugAssign(stmt)
    elif isinstance(stmt, ast.AnnAssign):
        stmt2 = AnnAssign(stmt)
    elif isinstance(stmt, ast.With):
        stmt2 = With(stmt)
    elif isinstance(stmt, ast.For):
        stmt2 = For(stmt)
    elif isinstance(stmt, ast.Try):
        stmt2 = Try(stmt)
    elif isinstance(stmt, ast.Return):
        stmt2 = Return(stmt)
    elif isinstance(stmt, ast.If):
        stmt2 = If(stmt)
    elif isinstance(stmt, ast.Expr):
        stmt2 = Expr(stmt)
    elif isinstance(stmt, ast.Pass):
        stmt2 = Pass()
    elif isinstance(stmt, ast.Continue):
        stmt2 = Continue()
    elif isinstance(stmt, ast.Break):
        stmt2 = Break()
    elif isinstance(stmt, ast.Assert):
        stmt2 = Assert(stmt)
    elif isinstance(stmt, ast.ClassDef):
        stmt2 = ClassDef(stmt)
    elif isinstance(stmt, ast.FunctionDef):
        stmt2 = FunctionDef(stmt)
    else:
        print(type(stmt))
        print(stmt._fields, [ type(x) for x in stmt._fields ])
        assert False
        stmt2 = None

    stmt2.parent = parent
    return stmt2

class Body(Statement):
    __slots__ = [ 'parent', 'statements' ]

    def __init__(self, parent, body):
        self.parent = parent
        self.statements = [ to_stmt(self, x) for x in body ]

    def __str__(self):
        return ''.join(str(x) for x in self.statements)

    def str_indent(self):
        s = str(self)

        return '\n'.join('\t%s' % x for x in s.split('\n')) + '\n'

            

class FunctionDef(Obj):
    __slots__ = [ 'name', 'args', 'body', 'type' ]

    def __init__(self, fnc: ast.FunctionDef):
        self.name = fnc.name
        self.args = [ Arg(self, x) for x in fnc.args.args ]

        arg_types = [ x.type for x in self.args ]
        returns = to_type(term(self, fnc.returns))
        self.type = TupleType(arg_types + [returns])

        offset = len(self.args) - len(fnc.args.defaults)
        for i, x in enumerate(fnc.args.defaults):
            self.args[offset + i].default = term(self, x)

        self.body = Body(self, fnc.body)

    def __str__(self):
        header = 'def %s(%s):' % (self.name, ', '.join(str(x) for x in self.args) )
        return '\n%s\n%s\n' % (header, self.body.str_indent())


class ClassDef(Type):
    __slots__ = [ 'bases', 'name', 'fields' ]

    def __init__(self, cls: ast.ClassDef):
        self.name = cls.name
        self.bases = [ term(self, x) for x in cls.bases] 

        statements = [ to_stmt(self, x) for x in cls.body ]

        classes[self.name] = self

        stmt = statements[0]
        if isinstance(stmt, Assign) and len(stmt.targets) == 1 and \
            isinstance(stmt.targets[0], Name) and stmt.targets[0].id == '__slots__':

            self.slots = stmt
            self.fields = [ Field(self, x.s) for x in stmt.value.elts ]
        else:
            self.slots = None
            self.fields = []

        self.methods = [ x for x in statements if isinstance(x, FunctionDef) ]        

        for fnc in self.methods:
            assert fnc.args[0].name == 'self'
            fnc.args[0].type = self

        # def search_assign_self(obj):
        #     if isinstance(obj, Assign):
        #         if len(obj.targets) == 1 and isinstance(obj.targets[0], Attribute):
        #             attr = obj.targets[0]
        #             if isinstance(attr.value, Name):
        #                 if attr.value.id == 'self':
        #                     return True
        #     return False

        # v = find_cond(self.body, search_assign_self, [])
        # self.fields = [ Field(self, x) for x in v ]

    def __str__(self):
        if len(self.bases) == 0:
            header = 'class %s:' % self.name
        else:
            header = 'class %s(%s):' % (self.name, ', '.join(str(x) for x in self.bases) )

        body = ''
        if self.slots is not None:
            body += '\t' + str(self.slots)

        for method in self.methods:
            s = str(method)
            body += '\n'.join('\t%s' % x for x in s.split('\n')) + '\n'

        return '%s\n%s' % (header, body)


        # return '\n%s\n%s\n' % (header, self.body.str_indent())


classes : Dict[str, ClassDef] = collections.OrderedDict()

def term(parent, expr):
    if expr is None:
        return None

    if isinstance(expr, ast.Call):
        trm = Call(expr)
    elif isinstance(expr, ast.Attribute):
        trm = Attribute(expr)
    elif isinstance(expr, ast.Name):
        trm = Name(expr)
    elif isinstance(expr, ast.BinOp):
        trm = BinOp(expr)
    elif isinstance(expr, ast.BoolOp):
        trm = BoolOp(expr)
    elif isinstance(expr, ast.UnaryOp):
        trm = UnaryOp(expr)
    elif isinstance(expr, ast.NameConstant):
        trm = NameConstant(expr)
    elif isinstance(expr, ast.Str):
        trm = Str(expr)
    elif isinstance(expr, ast.ListComp):
        trm = ListComp(expr)
    elif isinstance(expr, ast.Compare):
        trm = Compare(expr)
    elif isinstance(expr, ast.List):
        trm = List_(expr)
    elif isinstance(expr, ast.Tuple):
        trm = Tuple_(expr)
    elif isinstance(expr, ast.Dict):
        trm = Dict_(expr)
    elif isinstance(expr, ast.Index):
        trm = Index(expr)
    elif isinstance(expr, ast.Slice):
        trm = Slice(expr)
    elif isinstance(expr, ast.Subscript):
        trm = Subscript(expr)
    elif isinstance(expr, ast.GeneratorExp):
        trm = GeneratorExp(expr)
    elif isinstance(expr, ast.Num):
        trm = Num(expr)
    elif isinstance(expr, bool):
        trm = Bool(expr)
    else:
        print(type(expr))
        print(expr._fields, [ type(x) for x in expr._fields ])
        assert False

    trm.parent = parent
    return trm


def dmp(parent, obj):
    if not hasattr(obj, '__dict__'):
        return

    if isinstance(obj, Term) or isinstance(obj, Statement):
        assert obj.parent == parent
        
    items = get_fields(obj)
    for k, x in items:
        if k in [ 'parent', 'ctx', 'var', 'type' ]:
            continue
        if x is not None:
            if isinstance(x, Obj):

                dmp(obj, x)
            elif isinstance(x, list):
                for y in x:                    
                    dmp(obj, y)
            elif isinstance(x, str):
                pass
            elif isinstance(x, int):
                pass
            elif isinstance(x, float):
                pass
            elif isinstance(x, bool):
                pass
            else:
                print(type(x))

def get_fields(obj):
    if hasattr(obj, '__slots__') and (not hasattr(obj, '__dict__') or len(obj.__dict__) < len(obj.__slots__)):
        items = []
        for name in obj.__slots__:
            x = getattr(obj, name)
            items.append((name, x))
    else:
        items = obj.__dict__.items()

    items = [ (k, v) for k, v in items if not k in [ 'parent', 'ctx', 'var', 'type' ] ]
    return items


def navi_obj(obj, fnc, *args):
    fnc(obj)

    items = get_fields(obj)
    for _, x in items:
        if x is not None:
            if isinstance(x, Obj):

                navi_obj(x, fnc, args)
            elif isinstance(x, list):
                for y in x:                    
                    if isinstance(y, Obj):
                        navi_obj(y, fnc, args)


def navi_resolve_name(obj, vars):
    len_vars = len(vars)
    
    if isinstance(obj, FunctionDef):
        vars.insert(0, obj.args)
    elif isinstance(obj, For):
        vars.insert(0, obj.target)
    elif isinstance(obj, Comprehension):
        vars.insert(0, obj.target)

    items = get_fields(obj)
    for k, x in items:
        if k in [ 'parent', 'ctx', 'var', 'type' ]:
            continue
        if x is not None:
            if isinstance(x, Obj):

                navi_resolve_name(x, vars)
            elif isinstance(x, list):
                for y in x:                    
                    if isinstance(y, Obj):
                        navi_resolve_name(y, vars)

    obj.resolve_name(vars)

    if len_vars < len(vars):
        vars = vars[-len_vars:]
        assert len_vars == len(vars)

def setParenthesis(obj):
    if isinstance(obj, BinOp) or isinstance(obj, BoolOp):
        obj.with_parenthesis = False
        parent = obj.parent
        if isinstance(parent, UnaryOp):
            obj.with_parenthesis = True
        elif type(parent) == type(obj) and parent.op != obj.op and parent.getPrecedence() <= obj.getPrecedence():
            obj.with_parenthesis = True
