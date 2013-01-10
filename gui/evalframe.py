import pygtk
pygtk.require('2.0')
import gobject 
import gtk

from threading import *
import keybinding

from sets import *

import gtksourceview2
import pango

import sys

import storegraph

class EvalFrame(gtk.Frame, Thread, keybinding.KeyBinding):
    
    def __init__(self, _locals = None):
        gtk.Frame.__init__(self)
        self.set_label("Evaluator")
        
        # build a table to put all my stuffs
        self.table = gtk.Table(12, 16, True)
        self.table.show()
        self.add(self.table)

        #add an entry 
        self.entry = gtk.Entry()
        self.entry.set_can_focus(True)
        self.table.attach(self.entry, 0, 2, 0, 1)

        # build scrolled window and textview
        self.lm = gtksourceview2.LanguageManager()
        self.textbuffer = gtksourceview2.Buffer()
        self.textbuffer.set_data('languages-manager', self.lm)
        self.textview = gtksourceview2.View(self.textbuffer)
        self.textview.set_show_line_numbers(True)

        self.textbuffer.set_highlight_matching_brackets(True)

        language = self.lm.guess_language("p.py")
        if language:
            self.textbuffer.set_language(language)
            self.textbuffer.set_highlight_syntax(True)

        self.sw = gtk.ScrolledWindow()
        self.sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        self.sw.add(self.textview)
        self.sw.show()
        self.textview.show()
        self.table.attach(self.sw, 0, 10, 1, 8)

        # add an entry for expressions
        self.textbuffer3 = gtksourceview2.Buffer()
        self.textbuffer3.set_data('languages-manager', self.lm)
        self.textview3 = gtksourceview2.View(self.textbuffer3)

        self.sw3 = gtk.ScrolledWindow()
        self.sw3.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        self.sw3.add(self.textview3)
        self.sw3.show()
        self.textview3.show()
        self.table.attach(self.sw3, 2, 10, 0, 1)

        self.path = "code.py"
        try:
            txt = open(self.path).read()
            self.textbuffer.set_text(txt)
        except:
            None
                

        # build the result textview
        self.sw2 = gtk.ScrolledWindow()
        self.sw2.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        self.textview2 = gtk.TextView()
        self.textview2.set_editable(False)
        self.textbuffer2 = self.textview2.get_buffer()
        self.textview2.show()
        self.sw2.add(self.textview2)
        self.sw2.show()
        self.table.attach(self.sw2, 0, 10, 8, 12)

        #for locals
        if _locals == None:
            self.m_locals = locals()
        else:
            self.m_locals = _locals

        try:
            self.m_locals.add_callback(self.callback)
        except:
            pass                

        # view
        self.sw = gtk.ScrolledWindow()
        self.sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)

        self.liststore = gtk.ListStore(str, str, str, str)
        self.treeview = gtk.TreeView(self.liststore)

        self.fields = ["key", "formula", "type", "value"]

        self.columns = range(0, len(self.fields))
        self.cells = range(0, len(self.fields))                            

        for i in range(0, len(self.fields)):
            self.columns[i] = gtk.TreeViewColumn(self.fields[i])
            self.treeview.append_column(self.columns[i])
            self.cells[i] = gtk.CellRendererText()
            self.columns[i].pack_start(self.cells[i], True)
            self.columns[i].add_attribute(self.cells[i], 'text', i)

        self.sw.add(self.treeview)
        self.table.attach(self.sw, 10, 16, 0, 12)
        self.sw.show()
        self.treeview.show()

        self.treeview.set_enable_search(False)
        #self.liststore.set_enable_search(False)

        # dict of name to iterator
        self.key2iter = dict()

        self.treeview.connect("row-activated", self.local_clicked, None)

        self.vars = []

        for i in [self.textview3, self.textview, self.entry]:
            i.connect("key_press_event", self.key_pressed, None)
            i.connect("key_release_event", self.key_released, None)

        # initialize super class keybing
        keybinding.KeyBinding.__init__(self)
        self.ctrl = 65507

        # C-x C-c -> close the application
        self.keyactions.append(
            ([Set([65507, 120]), Set([65507,99])],
             lambda s: gtk.main_quit(),
             "close the application"
             )
            )

        # C-c C-k -> show local store
        self.keyactions.append(
            ([Set([65507, 99]), Set([65507,104])],
             lambda s: self.m_locals.show_graph(),
             "show the local store dependency graph"
             )
            )

        # C-c C-c -> execute
        self.keyactions.append(
            ([Set([65507, 99]), Set([65507,99])],
             lambda s: self.myexec(),
             "execute"
             )
            )

        # C-c C-n -> eval
        self.keyactions.append(
            ([Set([65507, 99]), Set([65507,110])],
             lambda s: self.myeval(),
             "evaluate"
             )
            )

        # C-k -> clear buffer and entry
        self.keyactions.append(
            ([Set([65507, 107])],
             lambda s: self.clear_buffers(),
             "clear buffer and entry"
             )
            )

        # C-h -> show keybindings
        self.keyactions.append(
            ([Set([65507, 104])],
             lambda s: self.textbuffer2.set_text(self.keycomments(gtk.gdk.keyval_name)),
             "show keybindings"
             )
            )

        # C-d -> remove a var
        self.keyactions.append(
            ([Set([65507, 100])],
             lambda s: self.del_var(),
             "delete a variable"
             )
            )

        self.get_settings().set_property("gtk-error-bell", False)

    def clear_buffers(self):
        self.textbuffer2.set_text("")  
        self.entry.set_text("")  
        self.textbuffer.set_text("")  
        return

    # key callback
    def key_pressed(self, widget, event, data=None):        
        
        self.keypressed(event.keyval)
        if event.state & gtk.gdk.CONTROL_MASK: self.keypressed(self.ctrl)
        #print str(event.keyval) + " from " + str(widget)
        #self.textview.grab_focus()
        return

    def key_released(self, widget, event, data=None):        
        self.keyreleased(event.keyval)
        if not (event.state & gtk.gdk.CONTROL_MASK): self.keyreleased(self.ctrl)
        #self.textview.grab_focus()
        return

    def callback(self, action, param):
        #print "evalframe.callback(" + str(action) + ", " + str(param) + ")"

        if action == "update" and param[0] in self.vars:
            key = param[0]
            
            try:
                formula = self.m_locals.formulas[key]
            except:
                formula = "no formula"

            try:
                keytype = type(self.m_locals.values[key])
            except:
                keytype = "no type"

            try:
                value = str(self.m_locals.values[key])
            except:
                value = "no value"


            if key not in self.key2iter.keys():
                self.key2iter[key] = self.liststore.append()

            self.liststore.set(self.key2iter[key], 0, str(key))
            self.liststore.set(self.key2iter[key], 1, formula)
            self.liststore.set(self.key2iter[key], 2, keytype)
            self.liststore.set(self.key2iter[key], 3, value)

        if action == "delete":
            key = param
            if key in self.key2iter.keys():
                self.liststore.remove(self.key2iter[key])
                del(self.key2iter[key])
                self.vars.remove(key)


        return

    def myexec(self, data=None):

        #m_str = self.textbuffer.get_text(self.textbuffer.get_start_iter(), self.textbuffer.get_end_iter())

        try:
            self.path = "code.py"
            try:
                open(self.path, "w").write(self.textbuffer.get_text(self.textbuffer.get_start_iter(), self.textbuffer.get_end_iter()))                
            except:
                None
            if "code" in sys.modules:
                reload(sys.modules["code"])
            self.m_locals.store_exec("from code import *")
            self.textbuffer2.set_text("") 

        except BaseException as e:
            if self.entry.get_text() <> "":
                self.vars.remove(self.entry.get_text())
            self.textbuffer2.set_text(str(e))        
            raise e


        self.textview.grab_focus()

    def myeval(self, data=None):

        if self.entry.get_text() <> "":
            # here we do an execution
            
            m_str = self.entry.get_text() + " = \"=" + self.textbuffer3.get_text(self.textbuffer3.get_start_iter(), self.textbuffer3.get_end_iter()).replace("\n", "\\\n").replace("\"","\\\"") + "\""
            self.vars.append(self.entry.get_text())

            try:
                self.m_locals.store_exec(m_str)
                self.textbuffer2.set_text("") 
                self.textbuffer3.set_text("") 
                self.entry.set_text("") 

            except BaseException as e:
                if self.entry.get_text() <> "":
                    self.vars.remove(self.entry.get_text())
                self.textbuffer2.set_text(str(e))        
                raise e


        else:


            m_str = self.textbuffer3.get_text(self.textbuffer3.get_start_iter(), self.textbuffer3.get_end_iter()).replace("\n", "\\\n")
            try:
                res = self.m_locals.store_eval(m_str)
                self.textbuffer2.set_text(str(res))        
                self.textbuffer3.set_text("")

            except BaseException as e:
                self.textbuffer2.set_text(str(e))        
                raise e
    
            self.textview3.grab_focus()



    def local_clicked(self, treeview, path, viewcolumn, data):
        #print "treeview: " + str(treeview) + " :: " + str(type(treeview))
        #print "path: " + str(path) + " :: " + str(type(path))
        #print "viewcolumn: " + str(viewcolumn) + " :: " + str(type(viewcolumn))
        #print "data: " + str(data) + " :: " + str(type(data))
        return

    def del_var(self):
        print self.treeview.get_cursor()[0]
        path = self.treeview.get_cursor()[0]
        if path <> None:
            piter = self.treeview.get_model().get_iter(path)
            varname = str(self.treeview.get_model().get_value(piter, 0))
            self.store.__delitem__(varname)


if __name__ == '__main__':
    
    ss = storegraph.Storegraph(_globals = globals())

    evalf = EvalFrame(ss)

    win = gtk.Window()
    win.add(evalf)

    win.connect('destroy', lambda win: gtk.main_quit())

    win.resize(800, 600)

    win.show_all()

    win.maximize()

    gtk.main()
