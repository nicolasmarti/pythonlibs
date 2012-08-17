import os, os.path
import sys
import pygtk
pygtk.require ('2.0')

import gtk
if gtk.pygtk_version < (2,10,0):
    print "PyGtk 2.10 or later required for this example"
    raise SystemExit

import gtksourceview2
import pango

from sets import *
import keybinding

import storegraph

# some error dialog
def error_dialog(parent, msg):
    dialog = gtk.MessageDialog(parent,
                               gtk.DIALOG_DESTROY_WITH_PARENT,
                               gtk.MESSAGE_ERROR,
                               gtk.BUTTONS_OK,
                               msg)
    dialog.run()
    dialog.destroy()

# a view implementing a simple editor a la proof general
# for a language implementing Lang interface and registered
# in the underlying python interpreter
class PG(gtksourceview2.View, keybinding.KeyBinding):
    
    def __init__(self, modulename, store = None):

        # store the module
        self.store = store

        if store == None:
            exec ("import " + modulename) in globals()
            self.module = eval (modulename)
            print "module loaded: " + str(self.module)
        else:
            self.store[modulename] = "=__import__(\"" + modulename + "\")"
            self.module = store[modulename]

        self.modulename = modulename

        # first define a buffer and its language manager
        self.lm = gtksourceview2.LanguageManager()
        self.buffer = gtksourceview2.Buffer()
        self.buffer.set_data('languages-manager', self.lm)

        # initialize super class using the buffer
        gtksourceview2.View.__init__(self, self.buffer)

        # attach key callback
        self.connect("key_press_event", self.key_pressed, None)
        self.connect("key_release_event", self.key_released, None)

        # initialize super class keybing
        keybinding.KeyBinding.__init__(self)
        self.ctrl = 65507

        # a simple keybinding for test
        # C-x C-c -> close the application
        self.keyactions.append(
            ([Set([65507, 120]), Set([65507,99])],
             lambda s: gtk.main_quit()
             )
            )

        # C-c C-n -> proceed next
        self.keyactions.append(
            ([Set([65507, 99]), Set([65507,110])],
             lambda s: s.proceed_definition()
             )
            )

        # C-c C-b -> proceed all
        self.keyactions.append(
            ([Set([65507, 99]), Set([65507,98])],
             lambda s: s.proceed_all()
             )
            )

        # C-c C-d -> print defs
        self.keyactions.append(
            ([Set([65507, 99]), Set([65507,100])],
             lambda s: s.show_defs()
             )
            )

        # C-c C-u -> undo last definition
        self.keyactions.append(
            ([Set([65507, 99]), Set([65507,117])],
             lambda s: s.undo()
             )
            )

        # C-c C-y -> undo all definitions
        self.keyactions.append(
            ([Set([65507, 99]), Set([65507,121])],
             lambda s: s.undo_all()
             )
            )

        # C-x C-f -> open a file
        self.keyactions.append(
            ([Set([65507, 120]), Set([65507,102])],
             lambda s: s.openfile()
             )
            )

        # C-x C-s -> save a file
        self.keyactions.append(
            ([Set([65507, 120]), Set([65507,115])],
             lambda s: s.savefile()
             )
            )

        # C-x C-k -> close a file
        self.keyactions.append(
            ([Set([65507, 120]), Set([65507,107])],
             lambda s: s.closefile()
             )
            )

        # the current starting position for position
        self.startpos = [0]

        # a tag for uneditable text 
        self.not_editable_tag = self.buffer.create_tag(editable=False, foreground="purple", background="green")

        # modifying theme for sake of readability
        self.modify_base(gtk.STATE_NORMAL, gtk.gdk.color_parse('black'))
        self.modify_text(gtk.STATE_NORMAL, gtk.gdk.color_parse('white'))

        # set source features
        self.buffer.set_highlight_matching_brackets(True)

        # this is the list of list of elements I added in the store
        self.added = []

        # special function that I do not want to add to the store
        self.reserved = dir(self.module)

        # syntax highlight
        #language = self.lm.guess_language("d.doudou")
        #if language:
        #    self.buffer.set_language(language)
        #    self.buffer.set_highlight_syntax(True)

        self.get_settings().set_property("gtk-error-bell", False)

    # remove all marks
    def remove_all_marks(self):
        begin, end = self.buffer.get_bounds()
        self.buffer.remove_source_marks(begin, end)
        self.startpos = [0]

    # key callback
    def key_pressed(self, widget, event, data=None):        
        self.keypressed(event.keyval)
        if event.state & gtk.gdk.CONTROL_MASK: self.keypressed(self.ctrl)
        #print event.keyval
        return

    def key_released(self, widget, event, data=None):        
        self.keyreleased(event.keyval)
        if not (event.state & gtk.gdk.CONTROL_MASK): self.keyreleased(self.ctrl)
        return

    # proceed next definition
    def proceed_definition(self):
        # grab the start iterator
        startpos = self.startpos[len(self.startpos) - 1]
        startiter = self.buffer.get_iter_at_offset(startpos)
        # grab the last iter of the buffer
        enditer = self.buffer.get_end_iter()
        # grab the text
        text = self.buffer.get_text(startiter, enditer)
        if text == "": return
        # proceed the term
        try:
            res = self.module.definition(text)
        except Exception as e:
            error_dialog(self.get_toplevel(), str(e))
            return
        
        # update starting position         
        endpos = startpos + res
        self.startpos.append(endpos)

        enditer = self.buffer.get_iter_at_offset(endpos)

        # create a texttag

        self.buffer.apply_tag(self.not_editable_tag, startiter, enditer)
        
        # and we set the cursor there
        self.buffer.place_cursor(enditer)

        # update the store if necessary
        if self.store <> None:
            added = []
            for i in self.added:
                added.extend(i)
            toadd = []
            for i in dir(self.module):

                if i[0] <> "_" and i not in added and (i not in self.store.formulas.keys() or 
                                                       self.store.formulas[i] == self.modulename + "." + i
                                                       ) and i not in self.reserved:
                    toadd.append(i)

            for i in toadd:
                self.store.__setitem__(i, "=" + self.modulename + "." + i) 

            self.added.append(toadd)

        #print self.startpos
        return True

    # proceed all definitions
    def proceed_all(self):
        while self.proceed_definition():
            None

    #show defs
    def show_defs(self):
        error_dialog(self.get_toplevel(), self.module.getdefs())

    # undo last definition
    def undo(self):
        if len(self.startpos) == 1: return
        # calling the undo
        try:
            self.module.undo_definition()
        except Exception as e:
            error_dialog(self.get_toplevel(), str(e))
            return
        
        # poping the last starting position
        oldendpos = self.startpos.pop()

        # removing the tag
        newendpos = self.startpos[len(self.startpos) - 1]

        oldenditer = self.buffer.get_iter_at_offset(oldendpos)
        newenditer = self.buffer.get_iter_at_offset(newendpos)
        
        self.buffer.remove_tag(self.not_editable_tag, newenditer, oldenditer)

        # and we set the cursor
        self.buffer.place_cursor(newenditer)

        # remove the added names
        if self.store <> None:
            last = self.added.pop()
            for i in last:
                del(self.store[i])

        #print self.startpos
        return True

    # undo all definitions
    def undo_all(self):
        while self.undo():
            None

    # reset the buffer
    def resetbuffer(self):
        # first we undo everything
        self.undo_all()
        # then we remove all marks
        self.remove_all_marks()


    # open file
    def openfile(self):
        self.filew = gtk.FileSelection("File selection")
    
        def close(w):
            self.filew.hide()

        def fileok(w):
            self.filew.hide()   
            path = self.filew.get_filename()
            self.buffer.begin_not_undoable_action()
            try:
                txt = open(path).read()
            except:
                return False

            # reset the buffer
            self.resetbuffer()

            self.buffer.set_text(txt)
            self.buffer.set_data('filename', path)
            self.buffer.end_not_undoable_action()

            self.get_toplevel().set_title(path)

            self.buffer.set_modified(False)
            self.buffer.place_cursor(self.buffer.get_start_iter())
            return True           
            
        self.filew.connect("destroy", close)
        self.filew.ok_button.connect("clicked", fileok)

        self.filew.show()

    def savefile(self):
        # grab the filename 
        filename = self.buffer.get_data('filename')
        # grab the text
        txt = self.buffer.get_text(self.buffer.get_start_iter(), self.buffer.get_end_iter())
        # if this is not None, just save
        if filename <> None:
            try:
                txt = open(filename, 'w').write(txt)
            except:
                return False
        else:
            self.filew = gtk.FileSelection("File selection")

            def close(w):
                self.filew.hide()

            def fileok(w):
                self.filew.hide()   
                path = self.filew.get_filename()
                self.buffer.begin_not_undoable_action()
                #try:
                open(path, 'w').write(txt)
                self.buffer.set_data('filename', path)
                #except:
                #    return False
                self.get_toplevel().set_title(path)
                return True

            self.filew.connect("destroy", close)
            self.filew.ok_button.connect("clicked", fileok)
            
            self.filew.show()

    def closefile(self):
        # reset the buffer
        self.resetbuffer()
        # reset the text
        self.buffer.set_text("")
        # set filename as None
        self.buffer.set_data('filename', None)


class PGFrame(gtk.Frame):

    def __init__(self, modulename, store = None):
        gtk.Frame.__init__(self)

        srcview = PG(modulename, store)

        sw = gtk.ScrolledWindow()
        sw.set_shadow_type(gtk.SHADOW_ETCHED_IN)
        sw.set_policy(gtk.POLICY_AUTOMATIC,
                      gtk.POLICY_AUTOMATIC)

        sw.add(srcview)
        
        self.add(sw)

if __name__ == '__main__':
    
    win = gtk.Window()

    win.add(PGFrame("Calculus"))    

    win.connect('destroy', lambda win: gtk.main_quit())

    win.resize(800, 600)

    win.show_all()

    win.maximize()

    gtk.main()
