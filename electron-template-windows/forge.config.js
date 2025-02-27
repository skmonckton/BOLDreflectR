module.exports = {
  makers: [
    {
      name: '@electron-forge/maker-zip'     
    },
    {
      name: '@electron-forge/maker-squirrel',
      config: {
        certificateFile: './cert.pfx',
        certificatePassword: process.env.CERTIFICATE_PASSWORD
      }
    }
  ],
  packagerConfig: {
    icon: 'reflectr' // no file extension required
  }
};