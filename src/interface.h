/*
 * Semigroups GAP package
 *
 * This file contains the interface from GAP to the C++ semigroups. 
 *
 */

#ifndef SEMIGROUPS_GAP_INTERFACE_H
#define SEMIGROUPS_GAP_INTERFACE_H 1

/*******************************************************************************
 * GAP headers
*******************************************************************************/

extern "C" {
  #include "src/compiled.h"
}

/*******************************************************************************
 * Class for the interface from a GAP semigroup to a C++ semigroup and back 
*******************************************************************************/

class InterfaceBase {
  public:
    virtual        ~InterfaceBase () {};
    virtual void   enumerate (Obj limit) = 0;
    virtual bool   is_done () = 0;
    virtual void   find (Obj data, Obj lookfunc, Obj start, Obj end) = 0;
    virtual size_t size () = 0;
    virtual size_t current_size () = 0;
    virtual size_t nrrules () = 0;
    virtual void   right_cayley_graph (Obj data) = 0;
    virtual void   left_cayley_graph (Obj data) = 0;
    virtual void   elements (Obj data, Obj limit) = 0;
    virtual Obj    position (Obj data, Obj x) = 0;
    virtual void   word (Obj data, Obj pos) = 0;
    virtual void   relations (Obj data) = 0;
};

/*******************************************************************************
 * Instantiate an Interface from a generic semigroup data
*******************************************************************************/

InterfaceBase* InterfaceFromData (Obj data);

#endif
