//===- TableTraits.h - Define How a Table Should Behave ---------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef ACSE_ADT_TABLETRAITS_H
#define ACSE_ADT_TABLETRAITS_H

namespace acse {

// This traits is used to describe a data type as it is a table. A
// specialization must define the following types for iterating over table rows:
//
// - iterator: an iterator to the table rows; if the table cannot be modified,
//   it should not be defined
//
// - const_iterator: a constant iterator to the table rows
//
// Moreover, the following static member functions should be defined:
//
// - static iterator begin(Ty &Table): get an iterator to the table first row;
//   if the table is a read only table, this member can be omitted
//
// - static iterator end(Ty &Table): get an iterator to the first invalid row of
//   the table -- i.e. the first line following the last line in the table; if
//   the table is a read only table, this member can be omitted
//
// - static const_iterator begin(const Ty &Table): get a constant iterator to the
//   table first row
//
// - static const_iterator end(const Ty& Table): get a constant iterator to the
//   table fist invalid row of the table -- i.e. the first line following the
//   last line of the table
//
// The traits must also defines the type of the row:
//
// - Row: the type of a table row
//
// Finally, the following static member functions allows to get table properties:
//
// - static unsigned GetColumns(const Ty &Table): get the number of columns in
//   each table row
//
// Please notice that since the types of the columns are in general different,
// it is not possible providing iterator to walk trough the table columns, or
// trough the element of a row.
template <typename Ty>
struct TableTraits { };

} // End namespace acse.

#endif // ACSE_ADT_TABLETRAITS_H
