import type { Config } from "tailwindcss";

export default {
  content: ["./index.html", "./src/**/*.{ts,tsx}"],
  theme: {
    extend: {
      boxShadow: {
        panel: "0 10px 30px rgba(16, 42, 67, 0.12)"
      }
    }
  },
  plugins: []
} satisfies Config;
