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

// Tag structure used to identify that a frame generic argument is not used.
struct ParsingFrameUnused { };

// Parsing algorithms are usually recursive, however when it comes the time of
// implementing them it is needed to switch to an iterative version in order to
// achieve good performance.
//
// The following templates defines different function call frames, so parsers
// can simulate recursive calls without using the implicit stack frame.

template<typename Ty, typename Tp = ParsingFrameUnused>
class ParsingFrame;

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

template <typename Ty>
class ParsingFrame<Ty, ParsingFrameUnused> {
public:
  ParsingFrame(const Ty &Only) : Only(Only) { }

  ParsingFrame(const ParsingFrame &That): Only(That.Only) { }

  const ParsingFrame &operator=(const ParsingFrame &That) {
    if(this != That)
      Only = That.Only;

    return *this;
  }

public:
  operator Ty &() { return Only; }
  operator const Ty &() const { return Only; }

private:
  Ty Only;
};

// To ease the process of instancing parsing frames, a family of utility
// functions is defined. The generic argument types of the parsing frame are
// inferred by the compiler starting from the static type of the function
// arguments.

template <typename Ty, typename Tp>
ParsingFrame<Ty, Tp> MakeParsingFrame(const Ty &First, const Tp &Second) {
  return ParsingFrame<Ty, Tp>(First, Second);
}

template <typename Ty>
ParsingFrame<Ty> MakeParsingFrame(const Ty &Only) {
  return ParsingFrame<Ty>(Only);
}

// Defines default implementation for frame traits.
template <typename Ty>
struct DefaultFrameTraits {
  static void Dispose(Ty &Frame) { }
};

// Frame traits are requested by the parsing stack in order to manage frames.
// The following static member functions must be defined by trait
// specializations:
//
// - static void Dispose(Ty &Frame): release resources held by the frame
//
// The DefaultFrameTraits class defines a set of reasonable default
// implementations for these functions.
template <typename Ty>
struct FrameTraits : public DefaultFrameTraits<Ty> { };

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
  // Normally, if a token stream has been parsed correctly, there are no frames
  // left on the stack. However, on errors it can happen that there are still
  // some frames stacked when the parsing stack is destroyed.
  //
  // The destructor keep care of disposing not yet unstacked frames. If a
  // particular frame implementation allocates memory on the heap, it can
  // specialize frame traits, and have memory freed here.
  ~ParsingStack() {
    for(iterator I = begin(), E = end(); I != E; ++I)
      FrameTraits<Ty>::Dispose(*I);
  }

public:
  Ty &top() { return Stack.back(); }
  Ty &bot() { return Stack.front(); }

  const Ty &top() const { return Stack.back(); }
  const Ty &bot() const { return Stack.front(); }

  void push(const Ty &Frame) { Stack.push_back(Frame); }
  void pop() { Stack.pop_back(); }

  size_t size() const { return Stack.size(); }
  bool empty() const { return Stack.empty(); }

public:
  void Dump(llvm::raw_ostream &OS = llvm::errs()) const;

private:
  Storage Stack;
};

// A set of default values for FramePrintTraits.
template <typename Ty>
struct DefaultFramePrintTraits {
  static std::string GetText(const Ty &Frame) {
    return "";
  }
};

// A trait used to get information about a frame to be printed. It must provide
// the following members:
//
// - static std::string GetText(const Ty &Frame): get the text of the frame to
//   be rendered. The returned text can contains newlines
//
// Derive from DefaultFramePrintTraits to access to pre-defined defaults.
template <typename Ty>
struct FramePrintTraits : public DefaultFramePrintTraits<Ty> { };

// An utility class used to print a stack.
template <typename Ty, size_t InternalSize>
class ParsingStackPrinter {
private:
  typedef FramePrintTraits<Ty> PrintTraits;

public:
  ParsingStackPrinter(const ParsingStack<Ty, InternalSize> &Stack)
    : Stack(Stack) { }

public:
  void PrintStack(llvm::raw_ostream &OS = llvm::errs()) {
    LoadRenderingBuffer();
    PrintRenderingBuffer(OS);
  }

public:
  void LoadRenderingBuffer() {
    typedef typename ParsingStack<Ty, InternalSize>::const_reverse_iterator
                                                     frame_iterator;
    typedef llvm::SmallVector<llvm::StringRef, 4>::const_iterator
                                                   line_iterator;

    Texts.clear();
    MaxWidth = 0;

    for(frame_iterator I = Stack.rbegin(), E = Stack.rend(); I != E; ++I) {
      const std::string &RawText = PrintTraits::GetText(*I);

      llvm::StringRef RawTextRef;
      llvm::SmallVector<llvm::StringRef, 4> Lines;

      RawTextRef = RawText;
      RawTextRef.split(Lines, "\n", -1, false);

      // Walk through all components of the frame to update the max width.
      for(line_iterator J = Lines.begin(), F = Lines.end(); J != F; ++J) {
        MaxWidth = std::max(MaxWidth, J->size());
      }

      // Push the string as returned to the trait on the rendering buffer. This
      // can be optimized by pushing each component of the frame into the
      // rendering buffer separately.
      Texts.push_back(RawText);
    }
  }

  void PrintRenderingBuffer(llvm::raw_ostream &OS) const {
    typedef std::vector<std::string>::const_iterator frame_iterator;
    typedef llvm::SmallVector<llvm::StringRef, 4>::const_iterator line_iterator;

    OS << "Top" << "\n";
    OS << "-+-" << "\n";

    // Trivial decoration for empty stacks.
    if(Texts.empty()) {
      OS << " | " << " Empty" << "\n";

    // For stacks with some elements, draw a frame around each of them.
    } else {
      PrintFrame(OS);

      for(frame_iterator I = Texts.begin(), E = Texts.end(); I != E; ++I) {
        llvm::StringRef RawText;
        llvm::SmallVector<llvm::StringRef, 4> Lines;

        RawText = *I;
        RawText.split(Lines, "\n", -1, false);

        for(line_iterator J = Lines.begin(), F = Lines.end(); J != F; ++J) {
          std::string Padding(MaxWidth - J->size(), ' ');
          OS << " | " << "| " << *J << Padding << " |\n";
        }

        PrintFrame(OS);
      }
    }

    OS << "-+-" << "\n";
    OS << "Bot" << "\n";
  }

  void PrintFrame(llvm::raw_ostream &OS) const {
    std::string InnerRep(MaxWidth + 2, '-');
    OS << " | " << '+' << InnerRep << '+' << "\n";
  }

private:
  const ParsingStack<Ty, InternalSize> &Stack;

  std::vector<std::string> Texts;
  size_t MaxWidth;
};

// The Dump method is implemented here so ParsingStackPrinter can be used.
template <typename Ty, size_t InternalSize>
void ParsingStack<Ty, InternalSize>::Dump(llvm::raw_ostream &OS) const {
  ParsingStackPrinter<Ty, InternalSize> Printer(*this);
  Printer.PrintStack(OS);
}

} // End namespace acse.

#endif // ACSE_PARSE_PARSINGSTACK_H
