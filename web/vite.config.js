import { defineConfig } from 'vite'
import { exec } from "child_process"
import pluginPurgeCss from "vite-plugin-purgecss-updated-v5";
import simpleHtmlPlugin from 'vite-plugin-simple-html'

function purescriptPlugin() {
    return {
        name: "purescript-watch",
        handleHotUpdate: function({ file, server }) {
            if (file.endsWith(".purs")) {
                console.log("PureScript file updated")
                const command =
                    "spago bundle --bundle-type module --source-maps --outfile=pure.js --offline --minify"

                exec(command, (error, stdout, stderr) => {
                    if (error) {
                        console.error(`Error: ${error.message}`)
                        return;
                    }

                    if (stderr) {
                        console.error(`stderr: ${stderr}`)
                        return;
                    }

                    console.log(`stdout: ${stdout}`)
                    server.ws.send({
                        type: "full-reload",
                        path: "*"
                    })


                })
            }
        }
    }
}

export default defineConfig({
    plugins: [
        purescriptPlugin(),
        pluginPurgeCss({
            variables: true
        })],
    server: {
        open: true,
        proxy: {
            '/api/v1/coffee-list.json': {
                target: 'http://localhost:9000/api/v1/coffee-list.json',
                changeOrigin: true,
            }
        }

    }
})
