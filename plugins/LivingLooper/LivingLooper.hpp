// LivingLooper.hpp
// Victor Shepardson (victor.shepardson@gmail.com)

// parts of this file are adapted from code by Andrew Fyfe and Bogdan Teleaga
// licensed under GPLv3
// https://github.com/Fyfe93/RAVE-audition/blob/main/Source/Rave.h

#pragma once

// note: preprocessor wants torch included first
#include <torch/script.h>

#include "SC_PlugIn.hpp"

// #define ATTR(NAME, GET) if (attr.name == "NAME") {this->NAME = attr.value.GET();} 


namespace LivingLooper {

// LLModel encapsulates the libtorch parts
struct LLModel {

  torch::jit::Module model;

  int sr;
  int block_size;
  int z_per_second;
  int latent_size;
  int n_loops;
  bool loaded;
    
  std::vector<torch::jit::IValue> inputs_rave;
  std::vector<torch::jit::IValue> inputs_empty;

  LLModel() {
    // at::init_num_threads();
    // at::set_num_threads(1);
    // torch::set_num_threads(1);

//    unsigned int num_threads = std::thread::hardware_concurrency();
    this->loaded=false;
    torch::jit::getProfilingMode() = false;
    c10::InferenceMode guard;
    torch::jit::setGraphExecutorOptimize(true);
    }
    
  void load(const std::string& rave_model_file) {
    // std::cout << "\"" <<rave_model_file << "\"" <<std::endl;
    try {
        c10::InferenceMode guard;
        this->model = torch::jit::load(rave_model_file);
        this->model.eval();
        // this->model = torch::jit::optimize_for_inference(this->model);
    }
    catch (const c10::Error& e) {
      // why no error when filename is bad?
        std::cout << e.what();
        std::cout << e.msg();
        std::cout << "error loading the model\n";
        return;
    }

    // support for Neutone models
    // if (this->model.hasattr("model")){
    //   this->model = this->model.attr("model").toModule();
    // }

    this->block_size = this->latent_size = this->sr = this->n_loops = -1;

    for (auto const& attr: model.named_attributes()) {
        if (attr.name == "n_loops") {this->n_loops = attr.value.toInt();} 
        if (attr.name == "block_size") {this->block_size = attr.value.toInt();} 
        if (attr.name == "sampling_rate") {this->sr = attr.value.toInt();} 
    }
    // this->z_per_second = this->sr / this->block_size;

    if ((this->block_size<=0) || 
        (this->n_loops<=0) || 
        // (this->latent_size<0) || 
        (this->sr<=0)){
      std::cout << "model load failed" << std::endl;
      return;
    }

    std::cout << "\tnumber of loops: " << this->n_loops << std::endl;
    std::cout << "\tblock size: " << this->block_size << std::endl;
    // std::cout << "\tlatent size: " << this->latent_size << std::endl;
    std::cout << "\tsample rate: " << this->sr << std::endl;

    c10::InferenceMode guard;
    inputs_rave.clear();
    inputs_rave.push_back(torch::IValue(0));
    inputs_rave.push_back(torch::ones({1,1,block_size}));
    inputs_rave.push_back(torch::IValue(0));

    this->loaded = true;
  }

  void reset () {
    c10::InferenceMode guard;
    this->model.get_method("reset")(inputs_empty);
  }

  void forward (float* input, int loop_idx, int oneshot, float* outBuffer) {
    c10::InferenceMode guard;

    inputs_rave[0] = torch::IValue(loop_idx); 

    inputs_rave[1] = torch::from_blob(
      input, block_size).reshape({1, 1, block_size});

    inputs_rave[2] = torch::IValue(oneshot); 

    // auto t_start = std::chrono::high_resolution_clock::now();
    const auto y = this->model(inputs_rave).toTensor();
    // auto elapsed = std::chrono::high_resolution_clock::now() - t_start;
		// std::cout << "model time: " << elapsed.count() * 1e-6 << " ms" << std::endl;

    auto acc = y.accessor<float, 3>();
    for(int j=0; j<acc.size(0); j++) {
      for(int i=0; i<acc.size(2); i++) {
        outBuffer[n_loops*i + j] = acc[j][0][i];
      }
    }
    // auto data = y.data_ptr<float>(); //n_loops x 1 x block_size
    // for (int i=0; i<block_size*n_loops; i++){
    //   outBuffer[i] = data[i];
    // }  
  }

};

class LivingLooper : public SCUnit {

  public:
    LivingLooper();
    ~LivingLooper();

    void write_zeros_kr();
    void write_zeros_ar(int i);
    void next(int nSamples);

    LLModel * model;
    static std::map<std::string, LLModel* > models;

    float * inBuffer; 
    size_t bufIdx;

    float * outBuffer;

    bool first_block_done;
    int filename_length;
    int ugen_inputs;
    int ugen_outputs;

};

} // namespace RAVE
