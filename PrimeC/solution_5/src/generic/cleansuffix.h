// unset all presets and variants
#ifndef KEEP_VARIANT
    #undef variant
    #undef variant_elements
    #undef variant_base_type_t
    #undef preset_uint64v8
    #undef preset_uint64v4
    #undef preset_uint64v2
    #undef preset_uint32v16
    #undef preset_uint32v8
    #undef preset_uint32v4
    #undef preset_uint32v2
    #undef preset_uint16v32
    #undef preset_uint16v16
    #undef preset_uint16v8
    #undef preset_uint16v4
    #undef preset_uint16v2
    #undef preset_uint8v32
    #undef preset_uint8v16
    #undef preset_uint8v8
    #undef preset_uint8v4
    #undef preset_uint8v2
#endif

// unset all what is defined by setsuffix
#undef variantsuffix
#undef unrollssuffix
#undef bitbucket_t
#undef suffix
#undef BITBUCKET_ELEMENTS
#undef BITBUCKET_BASE
#undef BITBUCKET_BYTEINDEX

