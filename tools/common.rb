CBLAS_HEADER_FILENAME = "<cblas.h>"
AUTOGEN = "{- This file is auto-generated.  Do not edit directly. -}"

def strip_ext(filename)
  filename.dup.sub!(/\.in$/, "") or \
    raise("wrong file extension (expected '.in')")
end

def write_file(filename, string)
  File.open(filename, "w") do |f|
    f.puts(string)
  end
end
