// LivingLooper.hpp
// Victor Shepardson (victor.shepardson@gmail.com)

#pragma once

#include "../../../living-looper-core/LLModel.hpp"

#include "SC_PlugIn.hpp"


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
