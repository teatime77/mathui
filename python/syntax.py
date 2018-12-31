import ast
import codecs
import numpy as np
from typing import List, Tuple
import collections

opr_dic = {
    ast.Eq : '==',
    ast.LtE : '<=',
    ast.Add : '+',
    ast.Mod : '%',
    ast.Not : 'not',
    ast.Is : 'is',
    ast.IsNot : 'is not',
    ast.NotEq : '!=',
    ast.Or : 'or',
    ast.And: 'and',
    ast.In : 'in'
}

classes = collections.OrderedDict()

a = True
b = True
c = True

if a:
    print('')
elif b:
    print('')
elif c:
    print('')
else:
    print('')
    

x = 1
x: float = 1.2
x = (1,2)
y = x[1]

def sub(a:int, b: int, e: List[Tuple[int,float]]):
    c = a + b
    return c

class Obj:
    pass

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
        return str(self.value)


class Subscript(Term):
    __slots__ = [ 'slice', 'value' ]

    def __init__(self, expr):
        self.slice = term(self, expr.slice)
        self.value = term(self, expr.value)


    def __str__(self):
        return str(self.value) + '[' + str(self.slice) + ']'

class Name(Term):
    __slots__ = [ 'id', 'var' ]

    def __init__(self, expr):
        self.id = expr.id
        self.var = None

    def __str__(self):
        return self.id

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

class BinOp(Term):
    __slots__ = [ 'left', 'op', 'right' ]

    def __init__(self, expr):
        self.left = term(self, expr.left)
        self.op   = opr_dic[type(expr.op)]
        self.right = term(self, expr.right)

    def __str__(self):
        return '%s %s %s' % (self.left, self.op, self.right)

class UnaryOp(Term):
    __slots__ = [ 'op', 'operand' ]

    def __init__(self, expr):
        self.op   = opr_dic[type(expr.op)]
        self.operand = term(self, expr.operand)

    def __str__(self):
        return '%s %s' % (self.op, self.operand)

class BoolOp(Term):
    __slots__ = [ 'op', 'values' ]

    def __init__(self, expr):
        self.op   = opr_dic[type(expr.op)]
        self.values = [ term(self, x) for x in expr.values ]

    def __str__(self):
        return (' %s ' % self.op).join(str(x) for x in self.values)

class Call(Term):
    __slots__ = [ 'func', 'args' ]

    def __init__(self, expr):
        self.func = term(self, expr.func)
        self.args = [ term(self, x) for x in expr.args]

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

class AnnAssign(Assign):
    __slots__ = [ 'targets', 'value', 'tp' ]

    def __init__(self, stmt):
        self.targets = [ term(self, stmt.target) ]
        self.value = term(self, stmt.value)
        self.tp = term(self, stmt.annotation)

    def __str__(self):
        return '%s: %s = %s\n' % (self.targets[0], self.tp, self.value)

class Comprehension:
    __slots__ = [ 'parent', 'iter', 'target', 'ifs' ]

    def __init__(self, parent, cmp):
        self.parent = parent
        self.iter = term(self, cmp.iter)
        self.target = term(self, cmp.target)
        self.ifs = Body(self, cmp.ifs)
        if len(cmp.ifs) != 0:
            print('')

    def __str__(self):
        header = 'for %s in %s' % (self.target, self.iter)
        return header
        

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
        if 2 <= len(stmt.orelse):
            print('')

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
        self.target = term(self, stmt.target)
        self.iter = term(self, stmt.iter)
        self.body = Body(self, stmt.body)

    def __str__(self):
        header = 'for %s in %s:' % (self.target, self.iter)
        return '%s\n%s' % (header, self.body.str_indent())


class Variable(Obj):
    def __init__(self):
        pass

class Arg(Variable):
    __slots__ = [ 'parent', 'name' ]

    def __init__(self, parent, arg: ast.arg):
        self.parent = parent
        self.name = arg.arg
        if arg.annotation is None:
            self.type = None
        else:
            self.type = term(self, arg.annotation)

    def __str__(self):
        if self.type is None:
            return self.name
        else:
            return '%s: %s' % (self.name, self.type)

class Field(Variable):
    __slots__ = [ 'parent', 'name' ]

    def __init__(self, parent, name):
        self.parent = parent
        self.name = name
        # attr = asn.targets[0]
        # self.name = attr.attr
        # attr.attr.var = self

class Body(Statement):
    __slots__ = [ 'parent', 'statements' ]

    def __init__(self, parent, body):
        self.parent = parent
        self.statements = []
        for stmt in body:
            if isinstance(stmt, ast.Import):
                stmt2 = Import(stmt)
            elif isinstance(stmt, ast.ImportFrom):
                stmt2 = ImportFrom(stmt)
            elif isinstance(stmt, ast.Assign):
                stmt2 = Assign(stmt)
            elif isinstance(stmt, ast.AnnAssign):
                stmt2 = AnnAssign(stmt)
            elif isinstance(stmt, ast.With):
                stmt2 = With(stmt)
            elif isinstance(stmt, ast.For):
                stmt2 = For(stmt)
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
            elif isinstance(stmt, ast.Assert):
                stmt2 = Assert(stmt)
            elif isinstance(stmt, ast.ClassDef):
                stmt2 = ClassDef(stmt)
            elif isinstance(stmt, ast.FunctionDef):
                stmt2 = FunctionDef(stmt)
            else:
                print(type(stmt))
                assert False

            stmt2.parent = self
            self.statements.append(stmt2)

    def __str__(self):
        return ''.join(str(x) for x in self.statements)

    def str_indent(self):
        s = str(self)

        return '\n'.join('\t%s' % x for x in s.split('\n')) + '\n'

            

class FunctionDef(Obj):
    __slots__ = [ 'name', 'args', 'body' ]

    def __init__(self, fnc: ast.FunctionDef):
        self.name = fnc.name
        self.args = [ Arg(self, x) for x in fnc.args.args ]

        self.body = Body(self, fnc.body)

    def __str__(self):
        header = 'def %s(%s):' % (self.name, ', '.join(str(x) for x in self.args) )
        return '\n%s\n%s\n' % (header, self.body.str_indent())


class ClassDef(Obj):
    __slots__ = [ 'bases', 'name', 'body', 'fields' ]

    def __init__(self, cls: ast.ClassDef):
        self.name = cls.name
        self.bases = [ term(self, x) for x in cls.bases] 
        self.body  = Body(self, cls.body)

        classes[self.name] = self

        stmt = self.body.statements[0]
        if isinstance(stmt, Assign) and len(stmt.targets) == 1 and \
            isinstance(stmt.targets[0], Name) and stmt.targets[0].id == '__slots__':

            self.fields = [ Field(self, x.s) for x in stmt.value.elts ]
        else:
            self.fields = []


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

        return '\n%s\n%s\n' % (header, self.body.str_indent())

path = 'C:\\usr\\prj\\xbrl-reader\\python\\xbrl_reader.py'
path = __file__
with codecs.open(path, 'r', 'utf-8') as f:
    source_text = f.read()

root = ast.parse(source_text)

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
        
    for k, x in obj.__dict__.items():
        if k in [ 'parent', 'ctx', 'var' ]:
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

def find_cond(obj, cond, search_list):
    if not hasattr(obj, '__dict__'):
        return search_list

    if cond(obj):
        search_list.append(obj)

    for k, x in obj.__dict__.items():
        if k in [ 'parent', 'ctx', 'var' ]:
            continue
        if x is not None:
            if isinstance(x, Obj):

                find_cond(x, cond, search_list)
            elif isinstance(x, list):
                for y in x:                    
                    find_cond(y, cond, search_list)

    return search_list

body = Body(None, root.body)
dmp(None, body)

with codecs.open('dmp.py', 'w', 'utf-8') as f:
    f.write('%s' % body)