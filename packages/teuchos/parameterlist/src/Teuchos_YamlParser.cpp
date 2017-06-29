// @HEADER
//
// ***********************************************************************
//
//                    Teuchos: Common Tools Package
//                 Copyright (2004) Sandia Corporation
//
// Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
// the U.S. Government retains certain rights in this software.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
// 1. Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright
// notice, this list of conditions and the following disclaimer in the
// documentation and/or other materials provided with the distribution.
//
// 3. Neither the name of the Corporation nor the names of the
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY SANDIA CORPORATION "AS IS" AND ANY
// EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
// PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SANDIA CORPORATION OR THE
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
// Questions? Contact
//                    Jonathan Hu       (jhu@sandia.gov)
//                    Andrey Prokopenko (aprokop@sandia.gov)
//                    Ray Tuminaro      (rstumin@sandia.gov)
//
// ***********************************************************************
//
// @HEADER

#ifndef TEUCHOS_YAMLPARSER_DEF_H_
#define TEUCHOS_YAMLPARSER_DEF_H_

#include <iostream>
#include <iomanip>
#include <ios>
#include <sstream>

#include "Teuchos_YamlParser_decl.hpp"
#include "Teuchos_XMLParameterListCoreHelpers.hpp"
#include "Teuchos_YamlParameterListCoreHelpers.hpp"
#include "Teuchos_TwoDArray.hpp"
#include "Teuchos_YAML.hpp"
#include "Teuchos_Array.hpp"
#include "Teuchos_TwoDArray.hpp"

namespace Teuchos {

using PLPair = std::pair<std::string, ParameterEntry>;
using Scalar = std::pair<bool, std::string>;

bool operator==(PLPair const&, PLPair const&) { return false; }
bool operator<<(std::ostream& os, PLPair const&) { return os; }

bool operator==(Scalar const&, Scalar const&) { return false; }
bool operator<<(std::ostream& os, Scalar const&) { return os; }

namespace YamlParameterList {

class Reader : public Teuchos::Reader {
 public:
  Reader():Teuchos::Reader(YAML::ask_reader_tables()) {
  }
  virtual ~Reader() {}
 protected:
  virtual void at_shift(any& result_any, int token, std::string& text) {
    using std::swap;
    switch (token) {
      case YAML::TOK_RAW:
      case YAML::TOK_SQUOTED:
      case YAML::TOK_DQUOTED: {
        Scalar& result = make_any_ref<Scalar>(result_any);
        swap(result.second, text);
        bool is_quoted = (token != YAML::TOK_RAW);
        result.first = is_quoted;
        break;
      }
    }
  }
  virtual void at_reduce(any& result_any, int prod, std::vector<any>& rhs) {
    using std::swap;
    switch (prod) {
      case YAML::PROD_DOC: {
        swap(result_any, rhs.at(2));
        break;
      }
      case YAML::PROD_TOP_BMAP:
      case YAML::PROD_TOP_BSEQ:
      case YAML::PROD_BMAP: {
        swap(result_any, rhs.at(1));
        break;
      }
      case YAML::PROD_BSEQ: {
        nested_array_to_2d_array(result_any, rhs.at(1));
        break;
      }
      case YAML::PROD_TOP_BLOCK: {
        swap(result_any, rhs.at(0));
        break;
      }
      case YAML::PROD_BMAP_FIRST_ITEM:
      case YAML::PROD_FMAP_FIRST_ITEM: {
        map_first_item(result_any, rhs);
        break;
      }
      case YAML::PROD_BMAP_NEXT_ITEM:
      case YAML::PROD_FMAP_NEXT_ITEM: {
        map_next_item(result_any, rhs);
        break;
      }
      case YAML::PROD_BSEQ_FIRST_ITEM:
      case YAML::PROD_FSEQ_FIRST_ITEM: {
        seq_first_item(result_any, rhs);
        break;
      }
      case YAML::PROD_BSEQ_NEXT_ITEM:
      case YAML::PROD_FSEQ_NEXT_ITEM: {
        seq_next_item(result_any, rhs);
        break;
      }
      case YAML::PROD_BSEQ_SCALAR: {
        swap(result_any, rhs.at(2));
        break;
      }
      case YAML::PROD_BMAP_ITEM: {
        map_item(result_any, rhs.at(1), rhs.at(5));
        break;
      }
      case YAML::PROD_BMAP_SCALAR:
      case YAML::PROD_BMAP_BLOCK:
      case YAML::PROD_BMAP_FLOW: {
        swap(result_any, rhs.at(0));
        break;
      }
      case YAML::PROD_FSEQ_EMPTY: {
        throw ParserFail("Empty arrays (sequences) not allowed!"
            "\n(need to be able to deduce the member type)\n");
      }
      case YAML::PROD_FMAP_EMPTY: {
        make_any_ref<ParameterList>(result_any);
        break;
      }
      case YAML::PROD_FSEQ: {
        nested_array_to_2d_array(result_any, rhs.at(2));
        break;
      }
      case YAML::PROD_FMAP: {
        swap(result_any, rhs.at(2));
        break;
      }
      case YAML::PROD_FSEQ_SCALAR:
      case YAML::PROD_FSEQ_FLOW:
      case YAML::PROD_FMAP_SCALAR:
      case YAML::PROD_FMAP_FLOW: {
        swap(result_any, rhs.at(0));
        break;
      }
      case YAML::PROD_FMAP_ITEM: {
        map_item(result_any, rhs.at(0), rhs.at(4));
        break;
      }
    }
  }
  void map_item(any& result_any, any& key_any, any& value_any) {
    std::string& key = any_ref_cast<Scalar>(key_any).second;
    PLPair& result = make_any_ref<PLPair>(result_any);
    swap(result.first, key);
    if (value_any.type() == typeid(Scalar)) {
      Scalar& value_scalar = any_ref_cast<Scalar>(value_any);
      bool value_quoted = value_scalar.first;
      std::string& value_str = value_scalar.second;
      if (!value_quoted && is_parseable_as<int>(value_str)) {
        int value = parse_as<int>(value_str);
        result.second = ParameterEntry<int>(value);
      } else if (!value_quoted && is_parseable_as<double>(value_str)) {
        double value = parse_as<double>(value_str);
        result.second = ParameterEntry<double>(value);
      } else {
        result.second = ParameterEntry<std::string>(value_str);
      }
    } else if (value_any.type() == typeid(Array<int>)) {
      Array<int>& value = any_ref_cast<Array<int> >(value_any);
      result.second = ParameterEntry<Array<int> >(value);
    } else if (value_any.type() == typeid(Array<double>)) {
      Array<double>& value = any_ref_cast<Array<double> >(value_any);
      result.second = ParameterEntry<Array<double> >(value);
    } else if (value_any.type() == typeid(Array<std::string>)) {
      Array<std::string>& value = any_ref_cast<Array<std::string> >(value_any);
      result.second = ParameterEntry<Array<std::string> >(value);
    } else if (value_any.type() == typeid(ParameterList)) {
      ParameterList& value = any_ref_cast<ParameterList>(value_any);
      ParameterList& result_pl = result.second.setList();
      swap(result_pl, value);
    } else {
      throw ParserFail("unexpected YAML map value type");
    }
  }
  template <typename T>
  void nested_array_to_2d_array_tmpl(any& out, any& in) {
    Array<Array<T> >& inval = any_ref_cast<Array<Array<T> > >(in);
    TwoDArray<T>& outval = make_any_ref<TwoDArray<T> >(out);
    for (Teuchos_Ordinal i = 0; i < inval.size(); ++i) {
      if (inval[i].size() != inval[0].size()) {
        throw ParserFail("2D array: sub-arrays are different sizes");
      }
    }
    outval = TwoDArray<T>(inval.size(), inval[0].size());
    for (Teuchos_Ordinal i = 0; i < outval.numRows(); ++i) {
      for (Teuchos_Ordinal j = 0; j < outval.numCols(); ++j) {
        swap(outval(i, j), inval[i][j]);
      }
    }
  }
  void nested_array_to_2d_array(any& result_any, any& rhs_any) {
    using std::swap;
    if (rhs_any.type() == typeid(Array<Array<int> >)) {
      nested_array_to_2d_array_tmpl<int>(result_any, rhs_any);
    } else if (rhs_any.type() == typeid(Array<Array<double> >)) {
      nested_array_to_2d_array_tmpl<double>(result_any, rhs_any);
    } else if (rhs_any.type() == typeid(Array<Array<std::string> >)) {
      nested_array_to_2d_array_tmpl<std::string>(result_any, rhs_any);
    } else {
      swap(result_any, rhs_any);
    }
  }
  void map_first_item(any& result_any, std::vector<any>& rhs) {
    ParameterList& list = make_any_ref<ParameterList>(result_any);
    PLPair& pair = any_ref_cast<PLPair>(rhs.at(0));
    list.set(pair.first, pair.second);
  }
  void map_next_item(any& result_any, std::vector<any>& rhs) {
    using std::swap;
    swap(result_any, rhs.at(0));
    ParameterList& list = any_ref_cast<ParameterList>(result_any);
    PLPair& pair = any_ref_cast<PLPair>(rhs.at(2));
    list.set(pair.first, pair.second);
  }
  void seq_first_item(any& result_any, std::vector<any>& rhs) {
    using std::swap;
    if (rhs.at(0).type() == typeid(Scalar)) {
      Scalar& scalar = any_ref_cast<Scalar>(rhs.at(0));
      bool scalar_quoted = scalar.first;
      std::string& scalar_str = scalar.second;
      if (!scalar_quoted && is_parseable_as<int>(scalar_str)) {
        Array<int>& a = make_any_ref<Array<int> >(result_any);
        int v = any_cast<int>(rhs.at(0));
        a.push_back(v);
      } else if (!scalar_quoted && is_parseable_as<double>(scalar_str)) {
        Array<double>& a = make_any_ref<Array<double> >(result_any);
        double v = any_cast<double>(rhs.at(0));
        a.push_back(v);
      } else {
        Array<std::string>& a = make_any_ref<Array<std::string> >(result_any);
        std::string& v = any_ref_cast<std::string>(rhs.at(0));
        a.push_back(std::string());
        swap(a.back(), v);
      }
    } else if (rhs.at(0).type() == typeid(Array<int>)) {
      Array<Array<int> >& a = make_any_ref<Array<Array<int> > >(result_any);
      Array<int>& v = any_ref_cast<Any<int> >(rhs.at(0));
      a.push_back(Array<int>());
      swap(a.back(), v);
    } else if (rhs.at(0).type() == typeid(Array<double>)) {
      Array<Array<double> >& a = make_any_ref<Array<Array<double> > >(result_any);
      Array<double>& v = any_ref_cast<Any<double> >(rhs.at(0));
      a.push_back(Array<double>());
      swap(a.back(), v);
    } else if (rhs.at(0).type() == typeid(Array<std::string>)) {
      Array<Array<std::string> >& a = make_any_ref<Array<Array<std::string> > >(result_any);
      Array<std::string>& v = any_ref_cast<Any<std::string> >(rhs.at(0));
      a.push_back(Array<std::string>());
      swap(a.back(), v);
    } else {
      throw Teuchos::ParserFail(
          "unexpected sequence item type");
    }
  }
  void seq_next_item(any& result_any, std::vector<any>& rhs) {
    using std::swap;
    swap(result_any, rhs.at(0));
    if (result_any.type() == typeid(Array<int>)) {
      Array<int>& a = any_ref_cast<Array<int> >(result_any);
      Scalar& val = any_ref_cast<Scalar>(rhs.at(2));
      if (val.first) throw ParserFail("string in integer array");
      int v = parse_as<int>(val.second);
      a.push_back(v);
    } else if (result_any.type() == typeid(Array<double>)) {
      Array<double>& a = any_ref_cast<Array<double> >(result_any);
      Scalar& val = any_ref_cast<Scalar>(rhs.at(2));
      if (val.first) throw ParserFail("string in integer array");
      double v = parse_as<double>(val.second);
      a.push_back(v);
    } else if (result_any.type() == typeid(Array<std::string>)) {
      Array<std::string>& a = any_ref_cast<Array<std::string> >(result_any);
      std::string& v = any_ref_cast<Scalar>(rhs.at(2)).second;
      a.push_back(std::string());
      swap(a.back(), v);
    } else if (result_any.type() == typeid(Array<Array<int> >)) {
      Array<Array<int> >& a = any_ref_cast<Array<Array<int> > >(result_any);
      Array<int>& v = any_ref_cast<Any<int> >(rhs.at(2));
      a.push_back(Array<int>());
      swap(a.back(), v);
    } else if (result_any.type() == typeid(Array<Array<double> >)) {
      Array<Array<double> >& a = any_ref_cast<Array<Array<double> > >(result_any);
      Array<double>& v = any_ref_cast<Any<double> >(rhs.at(2));
      a.push_back(Array<double>());
      swap(a.back(), v);
    } else if (result_any.type() == typeid(Array<Array<std::string> >)) {
      Array<Array<std::string> >& a =
        any_ref_cast<Array<Array<std::string> > >(result_any);
      Array<std::string>& v = any_ref_cast<Any<std::string> >(rhs.at(2));
      a.push_back(Array<std::string>());
      swap(a.back(), v);
    } else {
      throw Teuchos::ParserFail(
          "bug in YamlParameterList::Reader: unexpected sequence (array) type");
    }
  }
 private:
  template <typename T>
  bool is_parseable_as(std::string const& text) {
    std::istringstream ss(text);
    T value;
    return (ss >> value);
  }
  template <typename T>
  T parse_as(std::string const& text) {
    std::istringstream ss(text);
    T value;
    ss >> value;
    return value;
  }
};

}} // namespace Teuchos::YamlParameterList

namespace Teuchos
{

template<typename T> Teuchos::Array<T> getYamlArray(const YAML::Node& node)
{
  Teuchos::Array<T> arr;
  for(YAML::const_iterator it = node.begin(); it != node.end(); it++)
  {
    arr.push_back(quoted_as<T>(*it));
  }
  return arr;
}

void checkYamlTwoDArray(const YAML::Node& node, const std::string& key)
{
  for (YAML::const_iterator it = node.begin(); it != node.end(); ++it)
  {
    if (it->size() != node.begin()->size())
    {
      throw YamlSequenceError(std::string("TwoDArray \"") + key + "\" has irregular sizes");
    }
  }
}

template<typename T> Teuchos::TwoDArray<T> getYamlTwoDArray(const YAML::Node& node)
{
  Teuchos::TwoDArray<T> arr;
  typename Teuchos::TwoDArray<T>::size_type i, j;
  arr.resizeRows(node.size());
  arr.resizeCols(node.begin()->size());
  i = 0;
  for (YAML::const_iterator rit = node.begin(); rit != node.end(); ++rit)
  {
    j = 0;
    for (YAML::const_iterator cit = rit->begin(); cit != rit->end(); ++cit)
    {
      arr(i, j) = quoted_as<T>(*cit);
      ++j;
    }
    ++i;
  }
  return arr;
}

/* Helper functions */

void updateParametersFromYamlFile(const std::string& yamlFileName,
                                  const Teuchos::Ptr<Teuchos::ParameterList>& paramList)
{
  //load the YAML file in as a new param list
  Teuchos::RCP<Teuchos::ParameterList> updated = YAMLParameterList::parseYamlFile(yamlFileName);
  //now update the original list (overwriting values with same key)
  paramList->setParameters(*updated);
}

void updateParametersFromYamlCString(const char* const data,
                                     const Teuchos::Ptr<Teuchos::ParameterList>& paramList,
                                     bool overwrite)
{
  Teuchos::RCP<Teuchos::ParameterList> updated = YAMLParameterList::parseYamlText(data);
  if(overwrite)
  {
    paramList->setParameters(*updated);
  }
  else
  {
    paramList->setParametersNotAlreadySet(*updated);
  }
}

void updateParametersFromYamlString(const std::string& yamlData,
                                  const Teuchos::Ptr<Teuchos::ParameterList>& paramList,
                                  bool overwrite)
{
  Teuchos::RCP<Teuchos::ParameterList> updated = YAMLParameterList::parseYamlText(yamlData);
  if(overwrite)
  {
    paramList->setParameters(*updated);
  }
  else
  {
    paramList->setParametersNotAlreadySet(*updated);
  }
}

Teuchos::RCP<Teuchos::ParameterList> getParametersFromYamlFile(const std::string& yamlFileName)
{
  return YAMLParameterList::parseYamlFile(yamlFileName);
}

Teuchos::RCP<Teuchos::ParameterList> getParametersFromYamlString(const std::string& yamlStr)
{
  std::stringstream ss(yamlStr);
  return YAMLParameterList::parseYamlStream(ss);
}

void writeParameterListToYamlOStream(
  const ParameterList &paramList,
  std::ostream &yamlOut
  )
{
  YAMLParameterList::writeYamlStream(yamlOut, paramList);
}

void writeParameterListToYamlFile(
  const ParameterList &paramList,
  const std::string &yamlFileName
  )
{
  YAMLParameterList::writeYamlFile(yamlFileName, paramList);
}

std::string convertXmlToYaml(const std::string& xmlFileName)
{
  //load the parameter list from xml
  Teuchos::RCP<Teuchos::ParameterList> toConvert = Teuchos::getParametersFromXmlFile(xmlFileName);
  //replace the file extension ".xml" with ".yaml", or append it if there was no extension
  std::string yamlFileName;
  if(xmlFileName.find(".xml") == std::string::npos)
  {
    yamlFileName = xmlFileName + ".yaml";
  }
  else
  {
    yamlFileName = xmlFileName.substr(0, xmlFileName.length() - 4) + ".yaml";
  }
  YAMLParameterList::writeYamlFile(yamlFileName, *toConvert);
  return yamlFileName;
}

void convertXmlToYaml(const std::string& xmlFileName, const std::string& yamlFileName)
{
  //load the parameter list from xml
  Teuchos::RCP<Teuchos::ParameterList> toConvert = Teuchos::getParametersFromXmlFile(xmlFileName);
  //replace the file extension ".xml" with ".yaml", or append it if there was no extension
  YAMLParameterList::writeYamlFile(yamlFileName, *toConvert);
}

void convertXmlToYaml(std::istream& xmlStream, std::ostream& yamlStream)
{
  //read xmlStream into a string until EOF
  std::string xmlString(std::istreambuf_iterator<char>(xmlStream), {});
  //load the parameter list from xml
  Teuchos::RCP<Teuchos::ParameterList> toConvert = Teuchos::getParametersFromXmlString(xmlString);
  //replace the file extension ".xml" with ".yaml", or append it if there was no extension
  YAMLParameterList::writeYamlStream(yamlStream, *toConvert);
}

namespace YAMLParameterList
{

Teuchos::RCP<Teuchos::ParameterList> parseYamlText(const std::string& text)
{
  Teuchos::ParameterList pl;
  std::vector<YAML::Node> baseMap = YAML::LoadAll(text);
  return readParams(baseMap);
}

Teuchos::RCP<Teuchos::ParameterList> parseYamlText(const char* text)
{
  Teuchos::ParameterList pl;
  std::vector<YAML::Node> baseMap = YAML::LoadAll(text);
  return readParams(baseMap);
}

Teuchos::RCP<Teuchos::ParameterList> parseYamlFile(const std::string& yamlFile)
{
  std::vector<YAML::Node> baseMap = YAML::LoadAllFromFile(yamlFile);
  return readParams(baseMap);
}

Teuchos::RCP<Teuchos::ParameterList> parseYamlStream(std::istream& yaml)
{
  std::vector<YAML::Node> baseMap = YAML::LoadAll(yaml);
  return readParams(baseMap);
}

Teuchos::RCP<Teuchos::ParameterList> readParams(std::vector<YAML::Node>& lists)
{
  Teuchos::RCP<Teuchos::ParameterList> pl = rcp(new Teuchos::ParameterList); //pl is the root ParameterList to be returned
  //If there is exactly one element in "lists", assume it is the anonymous top-level parameter list
  //If there are more than one, place them all in the anonymous top-level list
  for(size_t i = 0; i < lists.size(); i++)
  {
    processMapNode(lists[i], *pl, true);
  }
  return pl;
}

void processMapNode(const YAML::Node& node, Teuchos::ParameterList& parent, bool topLevel)
{
  if(node.Type() != YAML::NodeType::Map)
  {
    throw YamlStructureError("All top-level elements of the YAML file must be maps.");
  }
  if(topLevel)
  {
    parent.setName("ANONYMOUS");
    processMapNode(node.begin()->second, parent);
  }
  else
  {
    for(YAML::const_iterator i = node.begin(); i != node.end(); i++)
    {
      //make sure the key type is a string
      if(i->first.Type() != YAML::NodeType::Scalar)
      {
        throw YamlKeyError("Keys must be plain strings");
      }
      //if this conversion fails and throws for any reason (shouldn't), let the caller handle it
      const std::string key = quoted_as<std::string>(i->first);
      processKeyValueNode(key, i->second, parent, topLevel);
    }
  }
}

void processKeyValueNode(const std::string& key, const YAML::Node& node, Teuchos::ParameterList& parent, bool topLevel)
{
  //node (value) type can be a map (for nested param lists),
  //a scalar (int, double, string), or a sequence of doubles (vector<double>)
  if(node.Type() == YAML::NodeType::Scalar)
  {
    try
    {
      parent.set(key, quoted_as<int>(node));
    }
    catch(...)
    {
      try
      {
        parent.set(key, quoted_as<double>(node));
      }
      catch(...)
      {
        try
        {
          std::string rawString = quoted_as<std::string>(node);
          if(rawString == "true")
          {
            parent.set<bool>(key, true);
          }
          else if(rawString == "false")
          {
            parent.set<bool>(key, false);
          }
          else
          {
            parent.set(key, rawString);
          }
        }
        catch(...)
        {
          throw YamlScalarError("YAML scalars must be int, double, bool or string.");
        }
      }
    }
  }
  else if(node.Type() == YAML::NodeType::Map)
  {
    if(topLevel)
    {
      processMapNode(node, parent);
    }
    else
    {
      Teuchos::ParameterList& sublist = parent.sublist(key);
      processMapNode(node, sublist);
    }
  }
  else if(node.Type() == YAML::NodeType::Sequence)
  {
    if (node.begin()->Type() == YAML::NodeType::Sequence) {
      checkYamlTwoDArray(node, key);
      YAML::Node const& first_value = *(node.begin()->begin());
      try
      {
        quoted_as<int>(first_value);
        parent.set(key, getYamlTwoDArray<int>(node));
      }
      catch(...)
      {
        try
        {
          quoted_as<double>(first_value);
          parent.set(key, getYamlTwoDArray<double>(node));
        }
        catch(...)
        {
          try
          {
            quoted_as<std::string>(first_value);
            parent.set(key, getYamlTwoDArray<std::string>(node));
          }
          catch(...)
          {
            throw YamlSequenceError(std::string("TwoDArray \"") + key + "\" must contain int, double, bool or string");
          }
        }
      }
    } else {
      YAML::Node const& first_value = *(node.begin());
      try
      {
        quoted_as<int>(first_value);
        parent.set(key, getYamlArray<int>(node));
      }
      catch(...)
      {
        try
        {
          quoted_as<double>(first_value);
          parent.set(key, getYamlArray<double>(node));
        }
        catch(...)
        {
          try
          {
            quoted_as<std::string>(first_value);
            parent.set(key, getYamlArray<std::string>(node));
          }
          catch(...)
          {
            throw YamlSequenceError(std::string("Array \"") + key + "\" must contain int, double, bool or string");
          }
        }
      }
    }
  }
  else if(node.Type() == YAML::NodeType::Null)
  {
    //treat NULL as empty string (not an error)
    parent.set(key, std::string());
  }
  else
  {
    //Undefined
    throw YamlUndefinedNodeError("Value type in a key-value pair must be one of: int, double, string, array, sublist.");
  }
}

void writeYamlStream(std::ostream& yaml, const Teuchos::ParameterList& pl)
{
  //warn the user if floats/doubles with integer values will be printed incorrectly
  auto flags = yaml.flags();
  //make temporary stringstream to test flags
  std::ostringstream testStream;
  testStream.flags(flags);
  double testVal = 1;
  testStream << testVal;
  bool popFlags = false;
  if(testStream.str() == "1")
  {
    //must add showpoint to flags while writing yaml
    //this will always disambiguate (double) n and n, even if stream precision is 0
    //prints as "n.0000" where the number of trailing zeros is the stream precision
    //note: in YAML, "5." is a double but not an int
    std::cout << "Warning: yaml stream format flags would confuse double with integer value with int.\n";
    std::cout << "Setting std::ios::showpoint on the stream to fix this (will restore flags when done)\n";
    auto flagsCopy = flags;
    flagsCopy |= std::ios::showpoint;
    popFlags = true;
  }
  yaml << "%YAML 1.1\n---\n";
  yaml << "ANONYMOUS:";         //original top-level list name is not stored by ParameterList
  if(pl.numParams() == 0)
  {
    yaml << " { }\n";
  }
  else
  {
    writeParameterList(pl, yaml, 2);
  }
  yaml << "...\n";
  //restore flags
  if(popFlags)
  {
    yaml.flags(flags);
  }
}

void writeYamlFile(const std::string& yamlFile, const Teuchos::ParameterList& pl)
{
  std::ofstream yaml(yamlFile);
  /* set default floating-point style:
     1. 17 decimal places to ensure the value remains the same
     2. scientific: this prevents floating-point values that happen
        to be integers from being printed as integers, because YAML
        will then read that value back typed as an integer.
   */
  yaml << std::scientific << std::setprecision(17);
  writeYamlStream(yaml, pl);
}

void writeParameterList(const Teuchos::ParameterList& pl, std::ostream& yaml, int indentLevel)
{
  if(pl.begin() == pl.end())
  {
    yaml << "{ }\n";
  }
  else
  {
    yaml << '\n';
    for(PLIter it = pl.begin(); it != pl.end(); it++)
    {
      writeParameter(pl.name(it), pl.entry(it), yaml, indentLevel);
    }
  }
}

template <typename T>
struct YamlWrite {
  static void write(T const& x, std::ostream& stream) {
    stream << x;
  }
};

template <>
struct YamlWrite<double> {
  static void write(double const& x, std::ostream& stream) {
    generalWriteDouble(x, stream);
  }
};

template <>
struct YamlWrite<std::string> {
  static void write(std::string const& x, std::ostream& stream) {
    generalWriteString(x, stream);
  }
};

template <typename T>
void writeYamlTwoDArray(Teuchos::TwoDArray<T> const& arr, std::ostream& stream)
{
  typename Teuchos::TwoDArray<T>::size_type i, j;
  stream << '[';
  for (i = 0; i < arr.getNumRows(); ++i)
  {
    if (i) stream << ", ";
    stream << '[';
    for (j = 0; j < arr.getNumCols(); ++j)
    {
      if (j) stream << ", ";
      YamlWrite<T>::write(arr(i, j), stream);
    }
    stream << ']';
  }
  stream << ']';
}

void writeParameter(const std::string& paramName, const Teuchos::ParameterEntry& entry, std::ostream& yaml, int indentLevel)
{
  for(int i = 0; i < indentLevel; i++)
  {
    yaml << ' ';
  }
  generalWriteString(paramName, yaml);
  yaml << ": ";
  if(entry.isList())
  {
    writeParameterList(Teuchos::getValue<Teuchos::ParameterList>(entry), yaml, indentLevel + 2);
    return;
  }
  else if(entry.isArray())
  {
    yaml << '[';
    if(entry.isType<Teuchos::Array<int> >())
    {
      Teuchos::Array<int>& arr = Teuchos::getValue<Teuchos::Array<int> >(entry);
      for(int i = 0; i < arr.size(); i++)
      {
        yaml << arr[i];
        if(i != arr.size() - 1)
          yaml << ", ";
      }
    }
    else if(entry.isType<Teuchos::Array<double> >())
    {
      Teuchos::Array<double>& arr = Teuchos::getValue<Teuchos::Array<double> >(entry);
      for(int i = 0; i < arr.size(); i++)
      {
        generalWriteDouble(arr[i], yaml);
        if(i != arr.size() - 1)
          yaml << ", ";
      }
    }
    else if(entry.isType<Teuchos::Array<std::string> >())
    {
      Teuchos::Array<std::string>& arr = Teuchos::getValue<Teuchos::Array<std::string> >(entry);
      for(int i = 0; i < arr.size(); i++)
      {
        generalWriteString(arr[i], yaml);
        if(i != arr.size() - 1)
          yaml << ", ";
      }
    }
    yaml << ']';
  }
  else if(entry.isTwoDArray())
  {
    if(entry.isType<Teuchos::TwoDArray<int> >())
    {
      writeYamlTwoDArray<int>(
          Teuchos::getValue<Teuchos::TwoDArray<int> >(entry), yaml);
    }
    else if(entry.isType<Teuchos::TwoDArray<double> >())
    {
      writeYamlTwoDArray<double>(
          Teuchos::getValue<Teuchos::TwoDArray<double> >(entry), yaml);
    }
    else if(entry.isType<Teuchos::TwoDArray<std::string> >())
    {
      writeYamlTwoDArray<std::string>(
          Teuchos::getValue<Teuchos::TwoDArray<std::string> >(entry), yaml);
    }
  }
  else if(entry.isType<int>())
  {
    yaml << Teuchos::getValue<int>(entry);
  }
  else if(entry.isType<double>())
  {
    generalWriteDouble(Teuchos::getValue<double>(entry), yaml);
  }
  else if(entry.isType<std::string>())
  {
    std::string& str = Teuchos::getValue<std::string>(entry);
    if(strchr(str.c_str(), '\n'))
    {
      //need explicit indentation so that indentation in the string is preserved
      yaml << "|2-\n";    
      //for each line, apply indent then print the line verbatim
      size_t index = 0;
      while(true)
      {
        size_t next = str.find('\n', index);
        for(int i = 0; i < indentLevel + 2; i++)
        {
          yaml << ' ';
        }
        if(next == std::string::npos)
        {
          yaml << str.substr(index, std::string::npos);
          break;
        }
        else
        {
          yaml << str.substr(index, next - index) << '\n';
        }
        index = next + 1;
      }
    }
    else
    {
      generalWriteString(str, yaml);
    }
  }
  else if(entry.isType<bool>())
  {
    yaml << (Teuchos::getValue<bool>(entry) ? "true" : "false");
  }
  yaml << '\n';
}

void generalWriteString(const std::string& str, std::ostream& yaml)
{
  if(stringNeedsQuotes(str))
  {
    yaml << '\'' << str << '\'';
  }
  else
  {
    yaml << str;
  }
}

void generalWriteDouble(double d, std::ostream& yaml)
{
  yaml << d;
}

template <typename T>
static bool canBeParsedAs(std::string const& s) {
  std::istringstream iss(s);
  T val;
  iss >> std::noskipws >> val;
  return iss.eof() && !iss.fail();
}

static bool containsSpecialCharacters(std::string const& s) {
  char const* const control_chars = ":{}[],&*#?|-<>=!%@\\";
  return s.find_first_of(control_chars) != std::string::npos;
}

bool stringNeedsQuotes(const std::string& s)
{
  return containsSpecialCharacters(s) ||
         canBeParsedAs<int>(s) ||
         canBeParsedAs<double>(s);
}

} //namespace YAMLParameterList

} //namespace Teuchos

#endif
