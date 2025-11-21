# Living Looper for SuperCollider

## Installation

- Install SuperCollider
- open SuperCollider
    - on Windows, you may need to run as administrator the first time
- install the NN.ar server plugin: https://github.com/elgiano/nn.ar
- `Quarks.install("https://github.com/victor-shepardson/living-looper-sc")`

### Manual install

- Locate the SuperCollider extension folder from the SuperCollider menu: `File -> open user support directory -> Extensions`
- Install SuperCollider plugins by placing them in this folder:
    - NN.ar: https://github.com/elgiano/nn.ar/releases/tag/v0.0.6-updated
        - download the zip file for your platform
    - Living Looper: https://github.com/victor-shepardson/living-looper-sc/releases/tag/v1.1.1
        - download the "Source Code" zip file
- In the SuperCollider menu select `Language -> reboot interpreter`
- then run the line `LivingLooper.new`