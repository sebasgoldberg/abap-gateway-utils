class ZCL_GATEWAY_UTILS definition
  public
  final
  create private .

public section.

    class-methods:

        class_constructor,

        get_instance
            returning
                value(rv_result) type ref to zcl_gateway_utils.

    methods:

        GET_RANGE_FROM_SEL_OPT_TABLE
          importing
            IV_FIELD type CSEQUENCE
            IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
            IV_TO_UPPERCASE type ABAP_BOOL default ABAP_FALSE
            IV_MAX_LENGTH type I optional
            IV_COMPLETE_WITH_ZEROS type ABAP_BOOL default ABAP_FALSE
            IV_SPACES_TO_ASTERISKS type ABAP_BOOL default ABAP_FALSE
          changing
            CR_RANGE type TABLE,

        GET_REQUEST_HEADER
          importing
            IV_SRV_RUNTIME type ref to /IWBEP/IF_MGW_CONV_SRV_RUNTIME
            IV_KEY type CSEQUENCE
          returning
            value(RV_RESULT) type STRING
          raising
            /IWBEP/CX_MGW_TECH_EXCEPTION,

        GET_VALUE_FROM_SEL_OPT_TABLE
          importing
            IV_FIELD type CSEQUENCE
            IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
          returning
            value(RV_RESULT) type STRING
          raising
            /IWBEP/CX_MGW_TECH_EXCEPTION,

        GET_VALUE_FROM_PARAMS
          importing
            IV_FIELD type CSEQUENCE
            IT_PARAMETER type /IWBEP/T_MGW_NAME_VALUE_PAIR
          returning
            value(RV_RESULT) type STRING
          raising
            /IWBEP/CX_MGW_TECH_EXCEPTION.

protected section.
    class-data:
                  lv_instance type ref to zcl_gateway_utils.
private section.
ENDCLASS.



CLASS ZCL_GATEWAY_UTILS IMPLEMENTATION.

  method class_constructor.
    create object lv_instance.
  endmethod.

  method get_instance.
    rv_result = lv_instance.
  endmethod.

  method GET_RANGE_FROM_SEL_OPT_TABLE.
*          importing
*            IV_FIELD type CSEQUENCE
*            IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
*            IV_TO_UPPERCASE type ABAP_BOOL default ABAP_FALSE
*            IV_MAX_LENGTH type I optional
*            IV_COMPLETE_WITH_ZEROS type ABAP_BOOL default ABAP_FALSE
*            IV_SPACES_TO_ASTERISKS type ABAP_BOOL default ABAP_FALSE
*          changing
*            CR_RANGE type TABLE,

    FIELD-SYMBOLS:
                   <LS_FILTER> TYPE LINE OF /IWBEP/T_MGW_SELECT_OPTION,
                   <LS_RANGE> TYPE ANY.

    DATA:
          LS_SELECT_OPTIONS TYPE LINE OF /IWBEP/T_COD_SELECT_OPTIONS.

    READ TABLE IT_FILTER_SELECT_OPTIONS
    WITH KEY PROPERTY = IV_FIELD
    ASSIGNING <LS_FILTER>.

    CHECK SY-SUBRC EQ 0.

    LOOP AT <LS_FILTER>-SELECT_OPTIONS INTO LS_SELECT_OPTIONS.
      APPEND INITIAL LINE TO CR_RANGE ASSIGNING <LS_RANGE>.

      IF IV_MAX_LENGTH IS SUPPLIED.
        IF STRLEN( LS_SELECT_OPTIONS-LOW ) GT IV_MAX_LENGTH.
          SHIFT LS_SELECT_OPTIONS-LOW LEFT DELETING LEADING '*'.
        ENDIF.
        IF STRLEN( LS_SELECT_OPTIONS-HIGH ) GT IV_MAX_LENGTH.
          SHIFT LS_SELECT_OPTIONS-HIGH LEFT DELETING LEADING '*'.
        ENDIF.
      ENDIF.

      IF IV_TO_UPPERCASE EQ ABAP_TRUE.
        TRANSLATE LS_SELECT_OPTIONS-LOW TO UPPER CASE.
        TRANSLATE LS_SELECT_OPTIONS-HIGH TO UPPER CASE.
      ENDIF.

      IF IV_COMPLETE_WITH_ZEROS EQ ABAP_TRUE AND
          LS_SELECT_OPTIONS-OPTION NE 'CP'.

          SHIFT LS_SELECT_OPTIONS-LOW RIGHT DELETING TRAILING SPACE.
          TRANSLATE LS_SELECT_OPTIONS-LOW USING ' 0'.

          IF LS_SELECT_OPTIONS-HIGH IS NOT INITIAL.
            SHIFT LS_SELECT_OPTIONS-HIGH RIGHT DELETING TRAILING SPACE.
            TRANSLATE LS_SELECT_OPTIONS-HIGH USING ' 0'.
          ENDIF.

      ENDIF.

      IF IV_SPACES_TO_ASTERISKS EQ ABAP_TRUE.
        TRANSLATE LS_SELECT_OPTIONS-LOW USING ' *'.
        IF LS_SELECT_OPTIONS-HIGH IS NOT INITIAL.
          TRANSLATE LS_SELECT_OPTIONS-HIGH USING ' *'.
        ENDIF.
      ENDIF.

      MOVE-CORRESPONDING LS_SELECT_OPTIONS TO <LS_RANGE>.

    ENDLOOP.

  endmethod.

  method GET_REQUEST_HEADER.
*          importing
*            IV_KEY type CSEQUENCE
*          returning
*            value(RV_RESULT) type STRING
*          raising
*            /IWBEP/CX_MGW_TECH_EXCEPTION,

    FIELD-SYMBOLS:
                   <LS_REQUEST_HEADER> TYPE LINE OF TIHTTPNVP.

    DATA:
        LV_DPF TYPE REF TO /IWBEP/IF_MGW_DP_FACADE,
        LT_REQUEST_HEADER TYPE TIHTTPNVP,
        LV_KEY TYPE STRING.

    LV_DPF = IV_SRV_RUNTIME->get_dp_facade( ).

    LT_REQUEST_HEADER = LV_DPF->GET_REQUEST_HEADER( ).

    LV_KEY = IV_KEY.

    TRANSLATE LV_KEY TO LOWER CASE.

    READ TABLE LT_REQUEST_HEADER
    WITH KEY NAME = LV_KEY
    ASSIGNING <LS_REQUEST_HEADER>.

    CHECK SY-SUBRC EQ 0.

    RV_RESULT = <LS_REQUEST_HEADER>-VALUE.

  endmethod.

  method GET_VALUE_FROM_SEL_OPT_TABLE.
*          importing
*            IV_FIELD type CSEQUENCE
*            IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
*          returning
*            value(RV_RESULT) type STRING
*          raising
*            /IWBEP/CX_MGW_TECH_EXCEPTION,

    FIELD-SYMBOLS:
                   <LS_FILTER> TYPE LINE OF /IWBEP/T_MGW_SELECT_OPTION,
                   <LS_RANGE> TYPE ANY,
                   <LS_SELECT_OPTIONS> TYPE LINE OF /IWBEP/T_COD_SELECT_OPTIONS.

    READ TABLE IT_FILTER_SELECT_OPTIONS
    WITH KEY PROPERTY = IV_FIELD
    ASSIGNING <LS_FILTER>.

    IF SY-SUBRC NE 0.
      RAISE EXCEPTION TYPE /IWBEP/CX_MGW_TECH_EXCEPTION.
    ENDIF.

    READ TABLE <LS_FILTER>-SELECT_OPTIONS
    ASSIGNING <LS_SELECT_OPTIONS>
    INDEX 1.

    IF SY-SUBRC NE 0.
      RAISE EXCEPTION TYPE /IWBEP/CX_MGW_TECH_EXCEPTION.
    ENDIF.

    RV_RESULT = <LS_SELECT_OPTIONS>-LOW.

  endmethod.

  method GET_VALUE_FROM_PARAMS.
*          importing
*            IV_FIELD type CSEQUENCE
*            IT_PARAMETER type /IWBEP/T_MGW_NAME_VALUE_PAIR
*          returning
*            value(RV_RESULT) type STRING
*          raising
*            /IWBEP/CX_MGW_TECH_EXCEPTION.

    FIELD-SYMBOLS:
                   <LS_PARAM> TYPE LINE OF /IWBEP/T_MGW_NAME_VALUE_PAIR.

    READ TABLE IT_PARAMETER
    WITH KEY NAME = IV_FIELD
    ASSIGNING <LS_PARAM>.

    IF SY-SUBRC NE 0.
      RAISE EXCEPTION TYPE /IWBEP/CX_MGW_TECH_EXCEPTION.
    ENDIF.

    RV_RESULT = <LS_PARAM>-VALUE.

  endmethod.

ENDCLASS.
