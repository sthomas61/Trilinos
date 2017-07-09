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
//                    Daniel Ibanez     (daibane@sandia.gov)
//                    Roscoe Bartlett   (rabartl@sandia.gov)
//
// ***********************************************************************
//
// @HEADER

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
#include "Teuchos_Reader.hpp"

namespace Teuchos {

struct PLPair {
  std::string key;
  ParameterEntry value;
};

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

struct Token {
  enum {
    STRING,
    DOUBLE,
    INT
  };
  int kind;
  std::string text;
  int get_type() const {
    if (kind != Teuchos::YAML::TOK_RAW) return STRING;
    if (is_parseable_as<int>(text)) return INT;
    if (is_parseable_as<double>(text)) return DOUBLE;
    return STRING;
  }
  std::string get_unescaped_text() const {
    if (kind == Teuchos::YAML::TOK_SQUOTED) {
      std::string result;
      int state = 0;
      for (std::size_t i = 1; i < text.size() - 1; ++i) {
        if (text[i] == '\'') {
          if (state == 0) state = 1;
          else if (state == 1) {
            result.push_back('\'');
            state = 0;
          }
        } else if (state != 0) {
          throw ParserFail("bug in singly quoted parsing: get_unescaped quote in scalar");
        } else {
          result.push_back(text[i]);
        }
      }
      return result;
    }
    if (kind == Teuchos::YAML::TOK_DQUOTED) {
      std::string result;
      int state = 0;
      for (std::size_t i = 1; i < text.size() - 1; ++i) {
        if (text[i] == '\\' && state == 0) state = 1;
        else {
          result.push_back(text[i]);
          if (state == 1) state = 0;
        }
      }
      return result;
    }
    return text;
  }
};

bool operator==(PLPair const&, PLPair const&) { return false; }
bool operator<<(std::ostream& os, PLPair const&) { return os; }

bool operator==(Token const&, Token const&) { return false; }
bool operator<<(std::ostream& os, Token const&) { return os; }

namespace YAMLParameterList {

class Reader : public Teuchos::Reader {
 public:
  Reader():Teuchos::Reader(Teuchos::YAML::ask_reader_tables()) {
  }
  virtual ~Reader() {}
 protected:
  virtual void at_shift(any& result_any, int token, std::string& text) {
    using std::swap;
    switch (token) {
      case Teuchos::YAML::TOK_RAW:
      case Teuchos::YAML::TOK_SQUOTED:
      case Teuchos::YAML::TOK_DQUOTED: {
        Token& result = make_any_ref<Token>(result_any);
        result.kind = token;
        swap(result.text, text);
        break;
      }
    }
  }
  virtual void at_reduce(any& result_any, int prod, std::vector<any>& rhs) {
    using std::swap;
    switch (prod) {
      case Teuchos::YAML::PROD_DOC: {
        swap(result_any, rhs.at(1));
        TEUCHOS_ASSERT(result_any.type() == typeid(ParameterList));
        break;
      }
      case Teuchos::YAML::PROD_TOP_BMAP:
      case Teuchos::YAML::PROD_TOP_BSEQ:
      case Teuchos::YAML::PROD_TOP_BLOCK: {
        swap(result_any, rhs.at(0));
        break;
      }
      case Teuchos::YAML::PROD_BMAP: {
        swap(result_any, rhs.at(1));
        TEUCHOS_ASSERT(result_any.type() == typeid(ParameterList));
        break;
      }
      case Teuchos::YAML::PROD_BSEQ: {
        swap(result_any, rhs.at(1));
        break;
      }
      case Teuchos::YAML::PROD_BMAP_FIRST_ITEM:
      case Teuchos::YAML::PROD_FMAP_FIRST_ITEM: {
        map_first_item(result_any, rhs);
        TEUCHOS_ASSERT(result_any.type() == typeid(ParameterList));
        break;
      }
      case Teuchos::YAML::PROD_BMAP_NEXT_ITEM: {
        TEUCHOS_ASSERT(rhs.size() == 2);
        map_next_item(result_any, rhs.at(0), rhs.at(1));
        TEUCHOS_ASSERT(result_any.type() == typeid(ParameterList));
        break;
      }
      case Teuchos::YAML::PROD_FMAP_NEXT_ITEM: {
        map_next_item(result_any, rhs.at(0), rhs.at(3));
        TEUCHOS_ASSERT(result_any.type() == typeid(ParameterList));
        break;
      }
      case Teuchos::YAML::PROD_BSEQ_FIRST_ITEM:
      case Teuchos::YAML::PROD_FSEQ_FIRST_ITEM: {
        seq_first_item(result_any, rhs);
        break;
      }
      case Teuchos::YAML::PROD_BSEQ_NEXT_ITEM: {
        seq_next_item(result_any, rhs.at(0), rhs.at(1));
        break;
      }
      case Teuchos::YAML::PROD_FSEQ_NEXT_ITEM: {
        seq_next_item(result_any, rhs.at(0), rhs.at(3));
        break;
      }
      case Teuchos::YAML::PROD_BSEQ_SCALAR: {
        TEUCHOS_ASSERT(rhs.size() == 5);
        swap(result_any, rhs.at(2));
        break;
      }
      case Teuchos::YAML::PROD_BMAP_ITEM: {
        map_item(result_any, rhs.at(1), rhs.at(5));
        break;
      }
      case Teuchos::YAML::PROD_BMAP_SCALAR: {
        swap(result_any, rhs.at(0));
        break;
      }
      case Teuchos::YAML::PROD_BMAP_BLOCK: {
        TEUCHOS_ASSERT(rhs.size() == 2);
        swap(result_any, rhs.at(1));
        TEUCHOS_ASSERT(result_any.type() == typeid(ParameterList));
        break;
      }
      case Teuchos::YAML::PROD_BMAP_FLOW: {
        swap(result_any, rhs.at(0));
        break;
      }
      case Teuchos::YAML::PROD_FSEQ_EMPTY: {
        throw ParserFail("Empty arrays (sequences) not allowed!"
            "\n(need to be able to deduce the member type)\n");
      }
      case Teuchos::YAML::PROD_FMAP_EMPTY: {
        make_any_ref<ParameterList>(result_any);
        break;
      }
      case Teuchos::YAML::PROD_FSEQ: {
        TEUCHOS_ASSERT(rhs.size() == 4);
        swap(result_any, rhs.at(2));
        break;
      }
      case Teuchos::YAML::PROD_FMAP: {
        TEUCHOS_ASSERT(rhs.size() == 4);
        swap(result_any, rhs.at(2));
        break;
      }
      case Teuchos::YAML::PROD_FSEQ_SCALAR:
      case Teuchos::YAML::PROD_FSEQ_FLOW:
      case Teuchos::YAML::PROD_FMAP_SCALAR:
      case Teuchos::YAML::PROD_FMAP_FLOW: {
        swap(result_any, rhs.at(0));
        break;
      }
      case Teuchos::YAML::PROD_FMAP_ITEM: {
        map_item(result_any, rhs.at(0), rhs.at(4));
        break;
      }
      case Teuchos::YAML::PROD_RAW:
      case Teuchos::YAML::PROD_DQUOTED:
      case Teuchos::YAML::PROD_SQUOTED: {
        swap(result_any, rhs.at(0));
        break;
      }
    }
  }
  void resolve_array(any& array_any) {
    int min_type = Token::INT;
    if (array_any.type() == typeid(Array<Token>)) {
      Array<Token>& tokens = any_ref_cast<Array<Token> >(array_any);
      if (tokens.size() == 0) {
        throw ParserFail("arrays can't be empty\n"
                         "(need to determine their element type)\n");
      }
      for (Teuchos_Ordinal i = 0; i < tokens.size(); ++i) {
        min_type = std::min(min_type, tokens[i].get_type());
      }
      if (min_type == Token::INT) {
        Array<int> result(tokens.size());
        for (Teuchos_Ordinal i = 0; i < tokens.size(); ++i) {
          result[i] = parse_as<int>(tokens[i].text);
        }
        array_any = result;
      } else if (min_type == Token::DOUBLE) {
        Array<double> result(tokens.size());
        for (Teuchos_Ordinal i = 0; i < tokens.size(); ++i) {
          result[i] = parse_as<double>(tokens[i].text);
        }
        array_any = result;
      } else if (min_type == Token::STRING) {
        Array<std::string> result(tokens.size());
        for (Teuchos_Ordinal i = 0; i < tokens.size(); ++i) {
          result[i] = tokens[i].get_unescaped_text();
        }
        array_any = result;
      }
    } else if (array_any.type() == typeid(Array<Array<Token> >)) {
      Array<Array<Token> >& tokens = any_ref_cast<Array<Array<Token> > >(array_any);
      for (Teuchos_Ordinal i = 0; i < tokens.size(); ++i) {
        if (tokens[i].size() != tokens[0].size()) {
          throw ParserFail("2D array: sub-arrays are different sizes");
        }
        for (Teuchos_Ordinal j = 0; j < tokens.size(); ++j) {
          min_type = std::min(min_type, tokens[i][j].get_type());
        }
      }
      if (min_type == Token::INT) {
        TwoDArray<int> result(tokens.size(), tokens[0].size());
        for (Teuchos_Ordinal i = 0; i < tokens.size(); ++i) {
          for (Teuchos_Ordinal j = 0; j < tokens[0].size(); ++j) {
            result(i, j) = parse_as<int>(tokens[i][j].text);
          }
        }
        array_any = result;
      } else if (min_type == Token::DOUBLE) {
        TwoDArray<double> result(tokens.size(), tokens[0].size());
        for (Teuchos_Ordinal i = 0; i < tokens.size(); ++i) {
          for (Teuchos_Ordinal j = 0; j < tokens[0].size(); ++j) {
            result(i, j) = parse_as<double>(tokens[i][j].text);
          }
        }
        array_any = result;
      } else if (min_type == Token::STRING) {
        TwoDArray<std::string> result(tokens.size(), tokens[0].size());
        for (Teuchos_Ordinal i = 0; i < tokens.size(); ++i) {
          for (Teuchos_Ordinal j = 0; j < tokens[0].size(); ++j) {
            result(i, j) = tokens[i][j].get_unescaped_text();
          }
        }
        array_any = result;
      }
    }
  }
  void map_item(any& result_any, any& key_any, any& value_any) {
    using std::swap;
    std::string key = any_ref_cast<Token>(key_any).get_unescaped_text();
    PLPair& result = make_any_ref<PLPair>(result_any);
    swap(result.key, key);
    resolve_array(value_any);
    if (value_any.type() == typeid(Token)) {
      Token& value_token = any_ref_cast<Token>(value_any);
      int value_type = value_token.get_type();
      if (value_type == Token::INT) {
        int value = parse_as<int>(value_token.text);
        result.value = ParameterEntry(value);
      } else if (value_type == Token::DOUBLE) {
        double value = parse_as<double>(value_token.text);
        result.value = ParameterEntry(value);
      } else {
        result.value = ParameterEntry(value_token.get_unescaped_text());
      }
    } else if (value_any.type() == typeid(Array<int>)) {
      Array<int>& value = any_ref_cast<Array<int> >(value_any);
      result.value = ParameterEntry(value);
    } else if (value_any.type() == typeid(Array<double>)) {
      Array<double>& value = any_ref_cast<Array<double> >(value_any);
      result.value = ParameterEntry(value);
    } else if (value_any.type() == typeid(Array<std::string>)) {
      Array<std::string>& value = any_ref_cast<Array<std::string> >(value_any);
      result.value = ParameterEntry(value);
    } else if (value_any.type() == typeid(TwoDArray<int>)) {
      TwoDArray<int>& value = any_ref_cast<TwoDArray<int> >(value_any);
      result.value = ParameterEntry(value);
    } else if (value_any.type() == typeid(TwoDArray<double>)) {
      TwoDArray<double>& value = any_ref_cast<TwoDArray<double> >(value_any);
      result.value = ParameterEntry(value);
    } else if (value_any.type() == typeid(TwoDArray<std::string>)) {
      TwoDArray<std::string>& value = any_ref_cast<TwoDArray<std::string> >(value_any);
      result.value = ParameterEntry(value);
    } else if (value_any.type() == typeid(ParameterList)) {
      ParameterList& value = any_ref_cast<ParameterList>(value_any);
      ParameterList& result_pl = result.value.setList();
      swap(result_pl, value);
    } else {
      std::string msg = "unexpected YAML map value type ";
      msg += value_any.type().name();
      msg += " for key \"";
      msg += result.key;
      msg += "\"\n";
      throw ParserFail(msg);
    }
  }
  void map_first_item(any& result_any, std::vector<any>& rhs) {
    ParameterList& list = make_any_ref<ParameterList>(result_any);
    PLPair& pair = any_ref_cast<PLPair>(rhs.at(0));
    list.set(pair.key, pair.value);
  }
  void map_next_item(any& result_any, any& items, any& next_item) {
    using std::swap;
    swap(result_any, items);
    ParameterList& list = any_ref_cast<ParameterList>(result_any);
    PLPair& pair = any_ref_cast<PLPair>(next_item);
    list.set(pair.key, pair.value);
  }
  void seq_first_item(any& result_any, std::vector<any>& rhs) {
    using std::swap;
    if (rhs.at(0).type() == typeid(Token)) {
      Array<Token>& a = make_any_ref<Array<Token> >(result_any);
      Token& v = any_ref_cast<Token>(rhs.at(0));
      a.push_back(Token());
      swap(a.back(), v);
    } else if (rhs.at(0).type() == typeid(Array<Token>)) {
      Array<Array<Token> >& a = make_any_ref<Array<Array<Token> > >(result_any);
      Array<Token>& v = any_ref_cast<Array<Token> >(rhs.at(0));
      a.push_back(Array<Token>());
      swap(a.back(), v);
    } else {
      throw Teuchos::ParserFail(
          "unexpected sequence item type");
    }
  }
  void seq_next_item(any& result_any, any& items, any& next_item) {
    using std::swap;
    swap(result_any, items);
    if (result_any.type() == typeid(Array<Token>)) {
      Array<Token>& a = any_ref_cast<Array<Token> >(result_any);
      Token& val = any_ref_cast<Token>(next_item);
      a.push_back(Token());
      swap(a.back(), val);
    } else if (result_any.type() == typeid(Array<Array<Token> >)) {
      Array<Array<Token> >& a = any_ref_cast<Array<Array<Token> > >(result_any);
      Array<Token>& v = any_ref_cast<Array<Token> >(next_item);
      a.push_back(Array<Token>());
      swap(a.back(), v);
    } else {
      throw Teuchos::ParserFail(
          "bug in YAMLParameterList::Reader: unexpected sequence (array) type");
    }
  }
 private:
};

}} // namespace Teuchos::YAMLParameterList

namespace Teuchos {

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
  if (overwrite) paramList->setParameters(*updated);
  else paramList->setParametersNotAlreadySet(*updated);
}

void updateParametersFromYamlString(const std::string& yamlData,
                                  const Teuchos::Ptr<Teuchos::ParameterList>& paramList,
                                  bool overwrite)
{
  Teuchos::RCP<Teuchos::ParameterList> updated = YAMLParameterList::parseYamlText(yamlData);
  if (overwrite) paramList->setParameters(*updated);
  else paramList->setParametersNotAlreadySet(*updated);
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
  if(xmlFileName.find(".xml") == std::string::npos) {
    yamlFileName = xmlFileName + ".yaml";
  } else {
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
  std::istreambuf_iterator<char> begin(xmlStream);
  std::istreambuf_iterator<char> end;
  //read xmlStream into a string until EOF
  std::string xmlString(begin, end);
  //load the parameter list from xml
  Teuchos::RCP<Teuchos::ParameterList> toConvert = Teuchos::getParametersFromXmlString(xmlString);
  //replace the file extension ".xml" with ".yaml", or append it if there was no extension
  YAMLParameterList::writeYamlStream(yamlStream, *toConvert);
}

namespace YAMLParameterList
{

Teuchos::RCP<Teuchos::ParameterList> parseYamlText(const std::string& text)
{
  any result_any;
  YAMLParameterList::Reader reader;
  reader.read_string(result_any, text, "parseYamlText(std::string)");
  ParameterList& pl = any_ref_cast<ParameterList>(result_any);
  return rcp(new ParameterList(pl));
}

Teuchos::RCP<Teuchos::ParameterList> parseYamlText(const char* text)
{
  any result_any;
  YAMLParameterList::Reader reader;
  reader.read_string(result_any, text, "parseYamlText(const char*)");
  ParameterList& pl = any_ref_cast<ParameterList>(result_any);
  return rcp(new ParameterList(pl));
}

Teuchos::RCP<Teuchos::ParameterList> parseYamlFile(const std::string& yamlFile)
{
  any result_any;
  YAMLParameterList::Reader reader;
  reader.read_file(result_any, yamlFile);
  ParameterList& pl = any_ref_cast<ParameterList>(result_any);
  return rcp(new ParameterList(pl));
}

Teuchos::RCP<Teuchos::ParameterList> parseYamlStream(std::istream& yaml)
{
  any result_any;
  YAMLParameterList::Reader reader;
  reader.read_stream(result_any, yaml, "parseYamlStream");
  ParameterList& pl = any_ref_cast<ParameterList>(result_any);
  return rcp(new ParameterList(pl));
}

void writeYamlStream(std::ostream& yaml, const Teuchos::ParameterList& pl)
{
  //warn the user if floats/doubles with integer values will be printed incorrectly
  std::ios_base::fmtflags flags = yaml.flags();
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
    std::ios_base::fmtflags flagsCopy = flags;
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
  std::ofstream yaml(yamlFile.c_str());
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
