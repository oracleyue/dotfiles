/* Locate in "~/.config/gtk-3.0/" to decrease title bar in gnome 3 */
/* INFO: valid for Gnome 3.19+ */


/* shrink headebars */
headerbar {
    min-height: 28px;
    padding-left: 2px;   /* same as childrens vertical margins for nicer proportions */
    padding-right: 2px;

    border-radius: 2px;  /* remove the rounded corners*/
}

headerbar entry,
headerbar spinbutton,
headerbar button,
headerbar separator {
    margin-top: 1px; /* same as headerbar side padding for nicer proportions */
    margin-bottom: 1px;
}

/* shrink ssd titlebars */
.default-decoration {
    min-height: 0;  /* let the entry and button drive the titlebar size */ 
    padding: 2px
}

.default-decoration .titlebutton {
    min-height: 16px; /* tweak these two props to reduce button size */
    min-width: 16px;

    border-radius: 1px;
}
