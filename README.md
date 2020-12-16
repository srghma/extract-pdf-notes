```bash
mkdir -p ~/.local/bin
curl -sSL https://raw.githubusercontent.com/v-sukt/extract-pdf-notes/master/extract_notes.sh -o ~/.local/bin/extract_notes && chmod a+x ~/.local/bin/extract_notes
echo $PATH | grep ~/.local/bin > /dev/null || echo "export PATH=$HOME/.local/bin:$PATH" >> ~/.bashrc && source ~/.bashrc
extract_notes "Sample File.pdf"
```

## Using local docker image

```
docker build -t extract_pdf_notes .
```
