import { defineConfig } from 'vite'
import scalaJS from './vite-scalajs.js'

const backend = process.env.BACKEND ?? 'http://localhost:8095'

export default defineConfig({
  // 画面(Scalactive)は sbt の web プロジェクトから読み込む
  plugins: [scalaJS({ cwd: '..', projectID: 'web' })],
  server: {
    // IPv4 で待ち受ける（LAN には公開しない）
    host: '127.0.0.1',
    proxy: { '/api': backend, '/auth': backend, '/ics': backend, '/mcp': backend },
  },
})
