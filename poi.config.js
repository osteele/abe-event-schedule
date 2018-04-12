module.exports = {
    presets: [
        require('poi-preset-elm')({
            loaderOptions: {
                debug: false,
                forceWatch: true
            }
        })
    ],
    env: {
        API_SERVER: process.env.API_SERVER
    },
    html: {
        title: 'ABE Event'
    }
};
