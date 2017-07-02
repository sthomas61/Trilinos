

ADD_SUBDIRECTORIES(
  ParameterList # Put this first to define a test-only library
  Conditions
  Dependencies
  FancyOutputting
  FunctionObjects
  ObjectBuilder
  Validators
  XML
  XMLParser
  )
IF(TeuchosParameterList_ENABLE_TeuchosParser)
  ADD_SUBDIRECTORIES(yaml)
ENDIF()
