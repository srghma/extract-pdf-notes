# container for easy use
FROM python:3

# ADD extract_pdf_notes.py /usr/bin/extract_pdf_notes
# chmod a+x /usr/bin/extract_pdf_notes && \
RUN pip install --no-cache-dir pdfminer.six beeprint
# ENTRYPOINT ["/usr/bin/extract_pdf_notes"]
