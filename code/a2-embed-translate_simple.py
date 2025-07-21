import sys
import json
import urllib.request
import urllib.parse
import time
import unicodedata

def normalize_utf8(text):
    """
    Normalize UTF-8 text to handle encoding issues
    """
    try:
        # Normalize Unicode to NFC form (canonical decomposition, then canonical composition)
        normalized = unicodedata.normalize('NFC', text)
        # Ensure it's valid UTF-8
        normalized.encode('utf-8')
        return normalized
    except (UnicodeError, UnicodeDecodeError, UnicodeEncodeError):
        # If normalization fails, try to clean the string
        try:
            # Remove or replace problematic characters
            cleaned = text.encode('utf-8', errors='ignore').decode('utf-8')
            return unicodedata.normalize('NFC', cleaned)
        except:
            # Last resort: return original text
            return text

def translate_with_mymemory(text, source='de', target='en'):
    """
    Translate using MyMemory API (free, no API key required)
    """
    try:
        # Normalize UTF-8 first
        normalized_text = normalize_utf8(text)
        
        # Encode the text
        encoded_text = urllib.parse.quote(normalized_text)
        
        # Build URL
        url = f"https://api.mymemory.translated.net/get?q={encoded_text}&langpair={source}|{target}"
        
        # Make request
        with urllib.request.urlopen(url, timeout=10) as response:
            result = json.loads(response.read().decode('utf-8'))
            
        if result['responseStatus'] == 200:
            translation = result['responseData']['translatedText']
            return translation
        else:
            return text  # Return original word if translation fails
            
    except Exception as e:
        print(f"Translation error: {e}", file=sys.stderr)
        return text  # Return original word on error

def translate_words(words):
    """Translate a list of German words to English"""
    translations = []
    
    for word in words:
        try:
            # Add a small delay to be respectful to the free API
            time.sleep(0.1)
            
            translation = translate_with_mymemory(word)
            translations.append(translation)
            
        except Exception as e:
            print(f"Error translating '{word}': {e}", file=sys.stderr)
            translations.append(word)  # Fallback to original word
    
    return translations

def main():
    if len(sys.argv) < 2:
        print("Usage: python translate_simple.py <words_json> or <json_file>", file=sys.stderr)
        sys.exit(1)
    
    # Get words from command line arguments or file
    input_arg = sys.argv[1]
    
    if input_arg.endswith('.json'):
        # File input
        try:
            with open(input_arg, 'r', encoding='utf-8') as f:
                words = json.load(f)
        except (IOError, json.JSONDecodeError) as e:
            print(f"Error reading JSON file: {e}", file=sys.stderr)
            sys.exit(1)
    elif input_arg.startswith('['):
        # JSON input
        try:
            words = json.loads(input_arg)
        except json.JSONDecodeError:
            print("Invalid JSON input", file=sys.stderr)
            sys.exit(1)
    else:
        # Individual arguments
        words = sys.argv[1:]
    
    # Translate words
    translations = translate_words(words)
    
    # Output as JSON
    result = {
        "german": words,
        "english": translations
    }
    print(json.dumps(result, ensure_ascii=False))

if __name__ == "__main__":
    main()
