//===- TablePrinter.h - Print Everything Looks Like a Table -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef ACSE_SUPPORT_TABLEPRINTER_H
#define ACSE_SUPPORT_TABLEPRINTER_H

#include "acse/ADT/TableTraits.h"

#include "llvm/Support/raw_ostream.h"

#include <vector>

namespace acse {

// TODO: comment.
template <typename Ty>
struct DefaultTablePrintTraits {
  static bool HasHeader(const Ty &Table) {
    return false;
  }

  static bool HasRowNames(const Ty &Table) {
    return false;
  }

  // TODO: remove from the trait, width must be computed by the printer.
  static unsigned GetRowNameWidth(const Ty &Table) {
    return 0;
  }

  static std::string GetRowNamesHeading(const Ty &Table) {
    return "RowNames";
  }

  static std::string GetRowName(const Ty &Table,
                                const typename TableTraits<Ty>::Row &Row) {
    return "";
  }

  // TODO: remove from the trait, width must be computed by the printer.
  static std::vector<unsigned>
  GetColumnWidths(const Ty &Table,
                  const typename TableTraits<Ty>::Row &Row) {
    return std::vector<unsigned>(TableTraits<Ty>::GetColumns(Table));
  }

  static std::vector<std::string> GetColumnHeadings(const Ty &Table) {
    return std::vector<std::string>(TableTraits<Ty>::GetColumns(Table));
  }

  static std::vector<std::string>
  GetRowTexts(const Ty &Table,
              const typename TableTraits<Ty>::Row &Row) {
    return std::vector<std::string>(TableTraits<Ty>::GetColumns(Table));
  }
};

// TODO: comment.
template <typename Ty>
struct TablePrintTraits : public DefaultTablePrintTraits<Ty> { };

// TODO: comment.
template <typename Ty>
class TablePrinter {
private:
    typedef TableTraits<Ty> TableTraits;
    typedef TablePrintTraits<Ty> PrintTraits;

public:
  TablePrinter(const Ty &Table) : Table(Table) { }

public:
  void PrintTable(llvm::raw_ostream &OS = llvm::errs()) {
    std::vector<unsigned> Widths;

    ComputeColumnWidths(Widths);

    PrintHeader(OS, Widths);
    PrintBody(OS, Widths);
  }

private:
  void ComputeColumnWidths(std::vector<unsigned> &Widths) {
    typedef typename TableTraits::const_iterator row_iterator;

    typedef std::vector<unsigned>::iterator width_iterator;
    typedef std::vector<unsigned>::const_iterator const_width_iterator;

    bool HasRowNames = PrintTraits::HasRowNames(Table);
    unsigned Columns = HasRowNames + TableTraits::GetColumns(Table);

    Widths.assign(Columns, 0);

    row_iterator I = TableTraits::begin(Table),
                 E = TableTraits::end(Table);

    for(; I != E; ++I)
    {
      const std::vector<unsigned> &RowWidths =
        PrintTraits::GetColumnWidths(Table, *I);

      width_iterator J = Widths.begin(),
                     F = Widths.end();

      const_width_iterator K = RowWidths.begin(),
                           T = RowWidths.end();
 
      if(HasRowNames)
        *J++ = std::max(*J, PrintTraits::GetRowNameWidth(Table));

      for(; J != F && K != T; ++J, ++K)
        *J = std::max(*J, *K);
    }
  }

  void
  PrintHeader(llvm::raw_ostream &OS,
              const std::vector<unsigned> &ColumnWidths) {
    typedef std::vector<std::string>::const_iterator heading_iterator;
    typedef std::vector<unsigned>::const_iterator width_iterator;

    if(!PrintTraits::HasHeader(Table))
      return;

    PrintFrame(OS, '+', '-', ColumnWidths);

    width_iterator I = ColumnWidths.begin(),
                   E = ColumnWidths.end();

    if(PrintTraits::HasRowNames(Table))
      PrintCell(OS, '|', PrintTraits::GetRowNamesHeading(Table), *I++);

    const std::vector<std::string> &Headings =
      PrintTraits::GetColumnHeadings(Table);

    heading_iterator J = Headings.begin(),
                     F = Headings.end();

    for(; J != F && I != E; ++J, ++I)
      PrintCell(OS, '|', *J, *I);

    OS << "|\n";

    PrintFrame(OS, '+', '-', ColumnWidths);
  }

  void
  PrintBody(llvm::raw_ostream &OS,
            const std::vector<unsigned> &ColumnWidths) {
    typedef typename TableTraits::const_iterator row_iterator;

    typedef std::vector<std::string>::const_iterator text_iterator;
    typedef std::vector<unsigned>::const_iterator width_iterator;

    bool HasRowNames = PrintTraits::HasRowNames(Table);

    row_iterator I = TableTraits::begin(Table),
                 E = TableTraits::end(Table);

    for(; I != E; ++I) {
      width_iterator J = ColumnWidths.begin(),
                     F = ColumnWidths.end();

      if(HasRowNames)
        PrintCell(OS, '|', PrintTraits::GetRowName(Table, *I), *J++);

      const std::vector<std::string> &Texts =
        PrintTraits::GetRowTexts(Table, *I);

      text_iterator K = Texts.begin(),
                    T = Texts.end();

      for(; K != T && J != F; ++K, ++J)
        PrintCell(OS, '|', *K, *J);

      OS << "|\n";
    }

    PrintFrame(OS, '+', '-', ColumnWidths);
  }

  void PrintFrame(llvm::raw_ostream &OS,
                  char Side,
                  char Inner,
                  const std::vector<unsigned> &ColumnWidths) {
    typedef std::vector<unsigned>::const_iterator iterator;

    iterator I = ColumnWidths.begin(),
             E = ColumnWidths.end();

    if(I == E)
      return;

    OS << Side;

    for(; I != E; ++I) {
      std::string InnerRep(*I + 2, Inner);
      OS << InnerRep << Side;
    }

    OS << "\n";
  }

  void PrintCell(llvm::raw_ostream &OS,
                 char Side,
                 const std::string &Text,
                 unsigned ColumnWidth) {
    std::string Padding(ColumnWidth - Text.size(), ' ');
    OS << "| " << Text << Padding << " ";
  }

private:
  const Ty &Table;
};

template <typename Ty>
llvm::raw_ostream &PrintTable(const Ty &Table,
                              llvm::raw_ostream &OS = llvm::errs()) {
  TablePrinter<Ty> Printer(Table);

  Printer.PrintTable(OS);

  return OS;
}

} // End namespace acse.

#endif // ACSE_SUPPORT_TABLEPRINTER_H
