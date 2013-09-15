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

// Defines a reasonable set of defaults value for the TablePrintTratis. Extend
// this class if you want to use them in a trait specialization.
template <typename Ty>
struct DefaultTablePrintTraits {
  static bool HasHeader(const Ty &Table) {
    return false;
  }

  static bool HasRowNames(const Ty &Table) {
    return false;
  }

  static std::string GetRowNamesHeading(const Ty &Table) {
    return "RowNames";
  }

  static std::string GetRowName(const Ty &Table,
                                const typename TableTraits<Ty>::Row &Row) {
    return "";
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

// This trait allows to describe how a table should be rendered. A trait
// specialization must define the following members:
//
// - static bool HasHeader(const Ty &Table): must return true if an header must
//   be printed
//
// - static bool HasRowNames(const Ty &Table): must return true if a label must
//    be printed for each table row
//
// - static std::string GetRowNamesHeading(const Ty &Table): must return the
// name of the row name column
//
// - static std::string GetRowName(const Ty &Table,
//                                 const typename TableTraits<Ty>::Row &Row):
//   get the name of the given row
//
// - static std::vector<std::string> GetColumnHeadings(const Ty &Table): get the
//   names of the columns
//
// - static std::vector<std::string>
//   GetRowTexts(const Ty &Table,
//               const typename TableTraits<Ty>::Row &Row): get the texts to be
//   put inside each column of the given row
template <typename Ty>
struct TablePrintTraits : public DefaultTablePrintTraits<Ty> { };

// Utility class used to print a table to a stream -- see TablePrintTraits.
template <typename Ty>
class TablePrinter {
private:
    typedef TableTraits<Ty> TableTraits;
    typedef TablePrintTraits<Ty> PrintTraits;

public:
  TablePrinter(const Ty &Table) : Table(Table) { }

public:
  void PrintTable(llvm::raw_ostream &OS = llvm::errs()) {
    LoadRenderingBuffer();
    PrintRenderingBuffer(OS);
  }

private:
  void LoadRenderingBuffer() {
    typedef typename TableTraits::const_iterator iterator;

    bool HasHeader = PrintTraits::HasHeader(Table);
    bool HasRowNames = PrintTraits::HasRowNames(Table);

    unsigned ColumnsCount = TableTraits::GetColumns(Table);

    Texts.clear();
    ColumnWidths.assign(HasRowNames + ColumnsCount, 0);

    if(HasHeader) {
      const std::vector<std::string> &Header =
        PrintTraits::GetColumnHeadings(Table);

      if(HasRowNames) {
        const std::string &NameHeader = PrintTraits::GetRowNamesHeading(Table);

        Texts.push_back(NameHeader);
        ColumnWidths[0] = NameHeader.size();
      }

      for(unsigned J = 0, F = ColumnsCount; J != F; ++J) {
        Texts.push_back(Header[J]);
        ColumnWidths[HasRowNames + J] = Header[J].size();
      }
    }

    for(iterator I = Table.begin(), E = Table.end(); I != E; ++I) {
      const std::vector<std::string> &Row = PrintTraits::GetRowTexts(Table, *I);

      if(HasRowNames) {
        const std::string &RowName = PrintTraits::GetRowName(Table, *I);

        Texts.push_back(RowName);
        ColumnWidths[0] = std::max(ColumnWidths[0], RowName.size());
      }

      for(unsigned J = 0, F = ColumnsCount; J != F; ++J) {
        Texts.push_back(Row[J]);
        ColumnWidths[HasRowNames + J] = std::max(ColumnWidths[HasRowNames + J],
                                                 Row[J].size());
      }
    }
  }

  void PrintRenderingBuffer(llvm::raw_ostream &OS) const {
    bool HasHeader = PrintTraits::HasHeader(Table);
    unsigned ColumnsCount = ColumnWidths.size();

    PrintFrame(OS);

    for(unsigned I = 0, E = Texts.size(); I != E; ++I) {
      const std::string &Text = Texts[I];
      std::string Padding(ColumnWidths[I % ColumnsCount] - Text.size(), ' ');

      OS << "| " << Texts[I] << Padding << " ";

      if(I % ColumnsCount == ColumnsCount - 1) {
        OS << "|\n";

        if(HasHeader && I == ColumnsCount - 1)
          PrintFrame(OS);
      }
    }

    PrintFrame(OS);
  }

  void PrintFrame(llvm::raw_ostream &OS) const {
    OS << '+';

    for(size_t I = 0, E = ColumnWidths.size(); I != E; ++I) {
      std::string InnerRep(ColumnWidths[I] + 2, '-');
      OS << InnerRep << '+';
    }

    OS << '\n';
  }

private:
  const Ty &Table;

  std::vector<std::string> Texts;
  std::vector<size_t> ColumnWidths;
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
