//===- ParsingStack.h - A Generic Stack for Parsing Algorithms --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef ACSE_PARSE_PARSINGSTACK_H
#define ACSE_PARSE_PARSINGSTACK_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

namespace acse {

// Parsing algorithms are usually recursive, however when it comes the time of
// implementing them it is needed to switch to an iterative version in order to
// achieve good performance.
//
// The following templates defines different function call frames, so parsers
// can simulate recursive calls without using the implicit stack frame.

template <typename Ty, typename Tp>
class ParsingFrame {
public:
  ParsingFrame(const Ty &First, const Tp &Second) : First(First),
                                                    Second(Second) { }

  ParsingFrame(const ParsingFrame &That) : First(That.First),
                                           Second(That.Second) { }

  const ParsingFrame &operator=(const ParsingFrame &That) {
    if(this != &That) {
      First = That.First;
      Second = That.Second;
    }

    return *this;
  }

public:
  Ty &GetFirst() { return First; }
  Tp &GetSecond() { return Second; }

  const Ty &GetFirst() const { return First; }
  const Tp &GetSecond() const { return Second; }

  void SetFirst(const Ty &Elt) { First = Elt; }
  void SetSecond(const Tp &Elt) { Second = Elt; }

private:
  Ty First;
  Tp Second;
};

// To ease the process of instancing parsing frames, a family of utility
// functions is defined. The generic argument types of the parsing frame are
// inferred by the compiler starting from the static type of the function
// arguments.

template <typename Ty, typename Tp>
ParsingFrame<Ty, Tp> MakeParsingFrame(const Ty &First, const Tp &Second) {
  return ParsingFrame<Ty, Tp>(First, Second);
}

// A parsing stack allows to simulate recursive calls, often found in parsing
// algorithms. Moreover, it provides some utility member functions to inspect
// its status. It also keep care of freeing memory when an error is encountered
// during parsing -- see destructor.
template <typename Ty, size_t InternalStorage>
class ParsingStack {
private:
  typedef llvm::SmallVector<Ty, InternalStorage> Storage;

public:
  typedef typename Storage::iterator iterator;
  typedef typename Storage::const_iterator const_iterator;

  typedef typename Storage::reverse_iterator reverse_iterator;
  typedef typename Storage::const_reverse_iterator const_reverse_iterator;

public:
  iterator begin() { return Stack.begin(); }
  iterator end() { return Stack.end(); }

  const_iterator begin() const { return Stack.begin(); }
  const_iterator end() const { return Stack.end(); }

  reverse_iterator rbegin() { return Stack.rbegin(); }
  reverse_iterator rend() { return Stack.rend(); }

  const_reverse_iterator rbegin() const { return Stack.rbegin(); }
  const_reverse_iterator rend() const { return  Stack.rend(); }

public:
  ParsingStack() { }

private:
  ParsingStack(const ParsingStack &That) LLVM_DELETED_FUNCTION;
  const ParsingStack &operator=(const ParsingStack &That) LLVM_DELETED_FUNCTION;

public:
  // TODO: implement + comment.
  ~ParsingStack() { }

public:
  void push(const Ty &Frame) { Stack.push_back(Frame); }
  void pop() { Stack.pop_back(); }

  size_t size() const { return Stack.size(); }
  bool empty() const { return Stack.empty(); }

public:
  void Dump(llvm::raw_ostream &OS = llvm::errs()) const {
    OS << "Top" << "\n";
    OS << "-+-" << "\n";

    // Trivial decoration for empty stacks.
    if(empty()) {
      OS << " | " << " Empty" << "\n";

    // For stacks with some elements, draw a frame around each of them.
    } else {
      unsigned MaxInnerWidth = 0;

      // First we need to render each item get the maximum line length.
      for(const_reverse_iterator I = rbegin(), E = rend(); I != E; ++I) {
        MaxInnerWidth = std::max(MaxInnerWidth, ComputeExpectedWidth(*I));
      }

      PrintFrame(OS, '+', '-', MaxInnerWidth);

      // Now that we actually know how many space we need, we can actually
      // render the stack.
      for(const_reverse_iterator I = rbegin(), E = rend(); I != E; ++I) {
        PrintItem(OS, *I, MaxInnerWidth);
        PrintFrame(OS, '+', '-', MaxInnerWidth);
      }
    }

    OS << "-+-" << "\n";
    OS << "Bot" << "\n";
  }

  unsigned ComputeExpectedWidth(const Ty &Frame) const;

  void PrintFrame(llvm::raw_ostream &OS,
                  char Side,
                  char Inner,
                  unsigned InnerWidth) const;
  void PrintItem(llvm::raw_ostream &OS,
                 const Ty &Frame,
                 unsigned InnerWidth) const;

private:
  Storage Stack;
};

// A set of default values for FramePrintTraits.
template <typename Ty>
struct DefaultFramePrintTraits {
  static unsigned GetWidth(const Ty &Frame) {
    return 0;
  }

  static std::string GetText(const Ty &Frame) {
    return "";
  }
};

// A trait used to get information about a frame to be printed. It must provide
// the following members:
//
// - static unsigned GetWidth(const Ty &Frame): get the expected width of the
//   frame to be rendered
//
// - static std::string GetText(const Ty &Frame): get the text of the frame to
//   be rendered. The returned text can contains newlines
//
// Derive from DefaultFramePrintTraits to access to pre-defined defaults.
template <typename Ty>
struct FramePrintTraits : public DefaultFramePrintTraits<Ty> { };

// Implement all the core member functions used to print the stack here, after
// the declaration of the traits related to the single frame.

template <typename Ty, size_t InternalSize>
unsigned
ParsingStack<Ty, InternalSize>::ComputeExpectedWidth(const Ty &Frame) const {
  return FramePrintTraits<Ty>::GetWidth(Frame);
}

template <typename Ty, size_t InternalSize>
void
ParsingStack<Ty, InternalSize>::PrintFrame(llvm::raw_ostream &OS,
                                           char Side,
                                           char Inner,
                                           unsigned InnerWidth) const {
  std::string InnerRep(InnerWidth + 2, Inner);

  OS << " | " << Side << InnerRep << Side << "\n";
}

template <typename Ty, size_t InternalSize>
void
ParsingStack<Ty, InternalSize>::PrintItem(llvm::raw_ostream &OS,
                                          const Ty &Frame,
                                          unsigned InnerWidth) const {
  typedef llvm::SmallVector<llvm::StringRef, 4>::const_iterator iterator;

  const std::string &RawText = FramePrintTraits<Ty>::GetText(Frame);

  llvm::StringRef RawTextRef;
  llvm::SmallVector<llvm::StringRef, 4> Lines;

  RawTextRef = RawText;
  RawTextRef.split(Lines, "\n", -1, false);

  for(iterator I = Lines.begin(), E = Lines.end(); I != E; ++I) {
    std::string Padding(InnerWidth - I->size(), ' ');
    OS << " | " << "| " << *I << Padding << " |\n";
  }
}

} // End namespace acse.

#endif // ACSE_PARSE_PARSINGSTACK_H
