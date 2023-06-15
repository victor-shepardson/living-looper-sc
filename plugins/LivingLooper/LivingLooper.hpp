// LivingLooper.hpp
// Victor Shepardson (victor.shepardson@gmail.com)

// parts of this file are adapted from code by Andrew Fyfe and Bogdan Teleaga
// licensed under GPLv3
// https://github.com/Fyfe93/RAVE-audition/blob/main/Source/Rave.h

#pragma once

// note: preprocessor wants torch included first
// #include <torch/script.h>
// #include "LLModel.hpp"
#include "../../../living-looper-core/LLModel.hpp"

#include "SC_PlugIn.hpp"

// #define ATTR(NAME, GET) if (attr.name == "NAME") {this->NAME = attr.value.GET();} 


namespace LivingLooper {

class LivingLooper : public SCUnit {

  public:
    LivingLooper();
    // ~LivingLooper();

    void write_zeros_kr();
    void write_zeros_ar(int i);
    void next(int nSamples);

    std::unique_ptr<LLModel> model;

    int filename_length;
};

} // namespace LivingLooper
