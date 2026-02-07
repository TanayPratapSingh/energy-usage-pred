import pymupdf4llm

# Convert PDF to Markdown
md_text = pymupdf4llm.to_markdown("Final_Energy_Usage_Report_With_Graphs.pdf")

# Save to file
with open("PROJECT_REPORT.md", "w", encoding="utf-8") as f:
    f.write(md_text)

print("Conversion complete! Check PROJECT_REPORT.md")
