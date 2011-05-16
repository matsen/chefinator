from __future__ import division
import fractions
import traceback
import urwid
import json
import re

fractions_regexp = re.compile(r'((?:[0-9/]+\s*)+)([a-z]*)\s*')

palette = [
    ('header', 'white', 'black'),
    ('ingredient focus', 'black', 'dark cyan', 'standout'),
    ('dialog', 'black', 'light gray'),
    ('dialog item', 'black', 'dark cyan'),
    ('dialog item focus', 'white', 'light blue'),
]

cup = fractions.Fraction(1)
count = -cup
qcup = cup / 4
oz = qcup / 2
tbsp = oz / 2
tsp = tbsp / 3
etsp = tsp / 8
drop = tsp / 96
pint = cup * 2
quart = pint * 2
gallon = quart * 4

suffixes = {
    '': count,
    'cup': cup, 'cups': cup, 'c': cup,
    'tbsp': tbsp, 'T': tbsp,
    'tsp': tsp, 't': tsp,
    'oz': oz,
    'pint': pint, 'pints': pint, 'pt': pint,
    'quart': quart, 'quarts': quart, 'qt': quart,
    'gallon': gallon, 'gallons': gallon, 'gal': gallon,
}

def reduce_units(i, units_above_cup=True, units_below_cup=True):
    if i < 0:
        return str(-i)
    values = []
    def reduce_unit(i, name, divisor, base=None):
        full, i = divmod(i, base or divisor)
        if full:
            if base is not None:
                full = (full * base) / divisor
            if full > 1 and full % 1 != 0:
                full, frac = divmod(full, 1)
                values.append('%s %s %s' % (full, frac, name))
            else:
                values.append('%s %s' % (full, name))
        return i
    if units_above_cup:
        i = reduce_unit(i, 'gallon', gallon)
        i = reduce_unit(i, 'quart', quart)
        i = reduce_unit(i, 'pint', pint)
    i = reduce_unit(i, 'cup', cup, qcup)
    if units_below_cup:
        i = reduce_unit(i, 'tbsp', tbsp)
        i = reduce_unit(i, 'tsp', tsp, etsp)
        i = reduce_unit(i, 'drops', drop)
    else:
        i = reduce_unit(i, 'oz', oz, etsp)
    return ', '.join(values)

class MeasurementEdit(urwid.Edit):
    def value(self):
        ret = 0
        text = self.get_edit_text()
        try:
            for f, s in fractions_regexp.findall(text):
                ret += sum(
                    fractions.Fraction(x) for x in f.split()) * suffixes[s]
        except ValueError:
            return 0
        else:
            return ret

class IngredientBox(urwid.FlowWidget):
    def __init__(self, parent):
        self.parent = parent
        self.name = urwid.Edit()
        self.measure = MeasurementEdit()
        self.result = urwid.Text('')
        self.columns = urwid.Columns([
            ('fixed', 3, urwid.Text(' * ')),
            ('weight', 1, self.measure),
            ('fixed', 1, urwid.Text('')),
            ('weight', 2, self.name),
            ('fixed', 3, urwid.Text(' = ')),
            ('weight', 3, self.result),
        ])
    
    def selectable(self):
        return True
    
    def get_cursor_coords(self, size):
        return self.columns.get_cursor_coords(size)
    
    def keypress(self, size, key):
        return self.columns.keypress(size, key)
    
    def render(self, size, focus=False):
        return self.columns.render(size, focus)
    
    def rows(self, size, focus=False):
        return self.columns.rows(size, focus)
    
    def update(self):
        self.result.set_text(reduce_units(
            self.measure.value() * -self.parent.multiplier.value(), 
            units_above_cup=False))

class IngredientManager(urwid.Pile):
    def __init__(self, multiplier):
        self.multiplier = multiplier
        self.contents = [self.make_box()]
        self.update_pile()
    
    def make_box(self):
        return urwid.AttrWrap(IngredientBox(self), None, 'ingredient focus')
    
    def update_all(self):
        for w in self.contents:
            w.update()
    
    def update_pile(self, focus_item=None):
        self.__super.__init__(self.contents, focus_item)
        self._invalidate()
    
    def keypress(self, size, key):
        if key == ']':
            self.contents.append(self.make_box())
            self.update_pile(self.focus_item)
        elif key == '[':
            if len(self.contents) > 1:
                idx = self.contents.index(self.focus_item)
                del self.contents[idx]
                self.update_pile(
                    self.contents[min(idx, len(self.contents) - 1)])
        else:
            return self.__super.keypress(size, key)
    
    def get_ingredients(self):
        ret = []
        for box in self.contents:
            ret.append([box.name.get_edit_text(), box.measure.get_edit_text()])
        return ret
    
    def load_ingredients(self, seq):
        self.contents = []
        for name, measure in seq:
            box = self.make_box()
            box.name.set_edit_text(name)
            box.measure.set_edit_text(measure)
            self.contents.append(box)
        self.update_pile()

class BoxPadding(urwid.WidgetDecoration):
    def __init__(self, base, horiz=2, vert=2, fillchar=' '):
        self.__super.__init__(base)
        self.horiz, self.vert = horiz, vert
        self.fillchar = fillchar
    
    def sizing(self):
        return set([urwid.BOX])
    
    def render(self, size, focus=False):
        width, height = size
        canvas = urwid.SolidCanvas(self.fillchar, width, height)
        orig_canvas = self._original_widget.render(
            (width - self.horiz * 2, height - self.vert * 2), focus)
        return urwid.CanvasOverlay(orig_canvas, canvas, self.horiz, self.vert)
    
    def keypress(self, size, key):
        width, height = size
        return self._original_widget.keypress(
            (width - self.horiz * 2, height - self.vert * 2), key)

def pad(w, pad=2):
    w = urwid.Padding(w, ('fixed left', pad), ('fixed right', pad))
    w = urwid.Filler(w, ('fixed top', pad), ('fixed bottom', pad))
    return w

class Dialog(urwid.FlowWidget):
    def __init__(self, cb, text):
        self.text = urwid.Text(text)
        self.cb = cb
        self.buttons = [
            urwid.AttrMap(
                urwid.Button(label, on_press=self._button_press), 
                'dialog item', 'dialog item focus')
            for label in self.get_dialog_buttons()]
        widgets = [
            ('flow', urwid.Text('')),
            ('flow', self.text),
            ('flow', urwid.Text('')),
        ]
        widgets.extend(self.get_extra_widgets())
        widgets.extend([
            urwid.Columns(self.buttons, dividechars=2),
            ('flow', urwid.Text('')),
        ])
        self.pile = urwid.AttrMap(urwid.Padding(urwid.Pile(widgets), 
            ('fixed left', 1), ('fixed right', 1)), 'dialog')
    
    def get_dialog_buttons(self):
        return 'OK',
    
    def get_extra_widgets(self):
        return []
    
    def _button_press(self, button):
        self.button_press(button.label)
    
    def button_press(self, label):
        pass
    
    def selectable(self):
        return True
    
    def get_cursor_coords(self, size):
        return self.pile.get_cursor_coords(size)
    
    def keypress(self, size, key):
        return self.pile.keypress(size, key)
    
    def render(self, size, focus=False):
        return self.pile.render(size, focus)
    
    def rows(self, size, focus=False):
        return self.pile.rows(size, focus)

class MessageDialog(Dialog):
    def button_press(self, label):
        self.cb()

class AskStringDialog(Dialog):
    def get_dialog_buttons(self):
        return 'Cancel', 'OK'
    
    def get_extra_widgets(self):
        self.input = urwid.Edit()
        return [
            ('flow', urwid.AttrMap(self.input, 
                'dialog item', 'dialog item focus')),
            ('flow', urwid.Text('')),
        ]
    
    def button_press(self, label):
        if label == 'Cancel':
            res = None
        else:
            res = self.input.get_edit_text()
        self.cb(res)
    
    def keypress(self, size, key):
        ret = self.__super.keypress(size, key)
        if ret == 'esc':
            self.cb(None)
        elif ret == 'enter':
            self.cb(self.input.get_edit_text())
        else:
            return ret

class MultiOverlayer(urwid.WidgetWrap):
    def __init__(self, w):
        self.overlay_stack = []
        self.__super.__init__(w)
    
    def overlay(self, top, *a, **kw):
        self.overlay_stack.append(self._w)
        self._w = urwid.Overlay(top, self._w, *a, **kw)
        self._invalidate()
    
    def unoverlay(self):
        self._w = self.overlay_stack.pop()
        self._invalidate()

def main():
    multiplier = MeasurementEdit(edit_text='1')
    manager = IngredientManager(multiplier)
    meta = urwid.Edit(multiline=True)
    meat = urwid.Pile([
        ('flow', urwid.Columns([
            ('fixed', 12, urwid.Text('Multiplier: ')),
            ('fixed', 8, multiplier),
        ])),
        ('flow', urwid.Divider()),
        ('flow', manager),
        ('flow', urwid.Divider()),
        urwid.Filler(meta, valign='top'),
    ], focus_item=2)
    footer = urwid.Text('ESC: quit; F1: save; F2: load')
    top = MultiOverlayer(urwid.Frame(BoxPadding(meat, vert=1), footer=footer))
    
    def msg(s):
        def _cb():
            top.unoverlay()
        top.overlay(MessageDialog(_cb, s), 'center', 50, 'middle', None)
    
    def with_get_string(prompt):
        def deco(f):
            def wrap(*a, **kw):
                def _cb(text):
                    top.unoverlay()
                    if text is not None:
                        f(text, *a, **kw)
                top.overlay(AskStringDialog(_cb, prompt), 
                    'center', 50, 'middle', None)
            return wrap
        return deco
    
    @with_get_string('Enter a filename to save to:')
    def save_recipe(filename):
        try:
            res = dict(
                multiplier=multiplier.get_edit_text(),
                ingredients=manager.get_ingredients(),
                meta=meta.get_edit_text(),
            )
            with open(filename, 'wb') as outf:
                json.dump(res, outf)
        except Exception, e:
            msg(traceback.format_exc())
        else:
            msg('Recipe saved to %r.' % filename)
    
    @with_get_string('Enter a filename to load from:')
    def load_recipe(filename):
        try:
            with open(filename, 'rb') as inf:
                data = json.load(inf)
            multiplier.set_edit_text(data.pop('multiplier'))
            manager.load_ingredients(data.pop('ingredients'))
            meta.set_edit_text(data.pop('meta', ''))
        except Exception, e:
            msg(traceback.format_exc())
    
    def unhandled(key):
        if key == 'tab':
            manager.update_all()
        elif key == 'f1':
            save_recipe()
        elif key == 'f2':
            load_recipe()
        elif key in ('f8', 'esc'):
            raise urwid.ExitMainLoop()
    
    loop = urwid.MainLoop(top, palette, unhandled_input=unhandled)
    loop.run()

if __name__ == '__main__':
    main()
