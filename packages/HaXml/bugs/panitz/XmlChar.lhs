<section title="Testing for Xml Characters">
This module provides some tests on unicode characters.
The productions are taken from the XQuery working draft od december 2001.
These functions can as well be used for a XML lexer.

<code><![CDATA[

> module XmlChar where

> import Char

]]></code></production>

<production nr="123">
<code><![CDATA[

> isNmstart c = c==' ' || isLetter c 

]]></code></production>

<production nr="124">
<code><![CDATA[

> isNmchar c 
>  =   isDigit c || c=='.'  || c=='-' || c=='_' || isLetter c || isCombiningChar c || isExtender c 

]]></code></production>


<production nr="211">
<code><![CDATA[

> isLetter c = isBaseChar c || isIdeographic c

]]></code></production>
<production nr="212">
<code><![CDATA[

> isBaseChar c 
>  =     0x0041<=n && (n<=0x005A  
>    ||  0x0061<=n && (n<=0x007A  
>    ||  0x00C0<=n && (n<=0x00D6  
>    ||  0x00D8<=n && (n<=0x00F6
>    ||  0x00F8<=n && (n<=0x00FF
>    ||  0x0100<=n && (n<=0x0131
>    ||  0x0134<=n && (n<=0x013E  
>    ||  0x0141<=n && (n<=0x0148  
>    ||  0x014A<=n && (n<=0x017E  
>    ||  0x0180<=n && (n<=0x01C3  
>    ||  0x01CD<=n && (n<=0x01F0  
>    ||  0x01F4<=n && (n<=0x01F5  
>    ||  0x01FA<=n && (n<=0x0217  
>    ||  0x0250<=n && (n<=0x02A8  
>    ||  0x02BB<=n && (n<=0x02C1  
>    ||  0x0386==n  
>    ||  0x0388<=n && (n<=0x038A  
>    ||  0x038C==n  
>    ||  0x038E<=n && (n<=0x03A1  
>    ||  0x03A3<=n && (n<=0x03CE  
>    ||  0x03D0<=n && (n<=0x03D6  
>    ||  0x03DA==n  
>    ||  0x03DC==n  
>    ||  0x03DE==n 
>    ||  0x03E0==n 
>    ||  0x03E2<=n && (n<=0x03F3 
>    ||  0x0401<=n && (n<=0x040C 
>    ||  0x040E<=n && (n<=0x044F 
>    ||  0x0451<=n && (n<=0x045C 
>    ||  0x045E<=n && (n<=0x0481 
>    ||  0x0490<=n && (n<=0x04C4 
>    ||  0x04C7<=n && (n<=0x04C8 
>    ||  0x04CB<=n && (n<=0x04CC 
>    ||  0x04D0<=n && (n<=0x04EB 
>    ||  0x04EE<=n && (n<=0x04F5 
>    ||  0x04F8<=n && (n<=0x04F9 
>    ||  0x0531<=n && (n<=0x0556 
>    ||  0x0559==n 
>    ||  0x0561<=n && (n<=0x0586 
>    ||  0x05D0<=n && (n<=0x05EA 
>    ||  0x05F0<=n && (n<=0x05F2 
>    ||  0x0621<=n && (n<=0x063A 
>    ||  0x0641<=n && (n<=0x064A 
>    ||  0x0671<=n && (n<=0x06B7 
>    ||  0x06BA<=n && (n<=0x06BE 
>    ||  0x06C0<=n && (n<=0x06CE 
>    ||  0x06D0<=n && (n<=0x06D3 
>    ||  0x06D5==n 
>    ||  0x06E5<=n && (n<=0x06E6 
>    ||  0x0905<=n && (n<=0x0939 
>    ||  0x093D==n 
>    ||  0x0958<=n && (n<=0x0961 
>    ||  0x0985<=n && (n<=0x098C 
>    ||  0x098F<=n && (n<=0x0990 
>    ||  0x0993<=n && (n<=0x09A8 
>    ||  0x09AA<=n && (n<=0x09B0 
>    ||  0x09B2==n 
>    ||  0x09B6<=n && (n<=0x09B9 
>    ||  0x09DC<=n && (n<=0x09DD 
>    ||  0x09DF<=n && (n<=0x09E1 
>    ||  0x09F0<=n && (n<=0x09F1 
>    ||  0x0A05<=n && (n<=0x0A0A 
>    ||  0x0A0F<=n && (n<=0x0A10 
>    ||  0x0A13<=n && (n<=0x0A28 
>    ||  0x0A2A<=n && (n<=0x0A30 
>    ||  0x0A32<=n && (n<=0x0A33 
>    ||  0x0A35<=n && (n<=0x0A36 
>    ||  0x0A38<=n && (n<=0x0A39 
>    ||  0x0A59<=n && (n<=0x0A5C 
>    ||  0x0A5E==n 
>    ||  0x0A72<=n && (n<=0x0A74 
>    ||  0x0A85<=n && (n<=0x0A8B 
>    ||  0x0A8D==n 
>    ||  0x0A8F<=n && (n<=0x0A91 
>    ||  0x0A93<=n && (n<=0x0AA8 
>    ||  0x0AAA<=n && (n<=0x0AB0 
>    ||  0x0AB2<=n && (n<=0x0AB3 
>    ||  0x0AB5<=n && (n<=0x0AB9 
>    ||  0x0ABD==n 
>    ||  0x0AE0==n 
>    ||  0x0B05<=n && (n<=0x0B0C 
>    ||  0x0B0F<=n && (n<=0x0B10 
>    ||  0x0B13<=n && (n<=0x0B28 
>    ||  0x0B2A<=n && (n<=0x0B30 
>    ||  0x0B32<=n && (n<=0x0B33 
>    ||  0x0B36<=n && (n<=0x0B39 
>    ||  0x0B3D==n 
>    ||  0x0B5C<=n && (n<=0x0B5D 
>    ||  0x0B5F<=n && (n<=0x0B61 
>    ||  0x0B85<=n && (n<=0x0B8A 
>    ||  0x0B8E<=n && (n<=0x0B90 
>    ||  0x0B92<=n && (n<=0x0B95 
>    ||  0x0B99<=n && (n<=0x0B9A 
>    ||  0x0B9C==n 
>    ||  0x0B9E<=n && (n<=0x0B9F 
>    ||  0x0BA3<=n && (n<=0x0BA4 
>    ||  0x0BA8<=n && (n<=0x0BAA 
>    ||  0x0BAE<=n && (n<=0x0BB5 
>    ||  0x0BB7<=n && (n<=0x0BB9 
>    ||  0x0C05<=n && (n<=0x0C0C 
>    ||  0x0C0E<=n && (n<=0x0C10 
>    ||  0x0C12<=n && (n<=0x0C28 
>    ||  0x0C2A<=n && (n<=0x0C33 
>    ||  0x0C35<=n && (n<=0x0C39 
>    ||  0x0C60<=n && (n<=0x0C61 
>    ||  0x0C85<=n && (n<=0x0C8C 
>    ||  0x0C8E<=n && (n<=0x0C90 
>    ||  0x0C92<=n && (n<=0x0CA8 
>    ||  0x0CAA<=n && (n<=0x0CB3 
>    ||  0x0CB5<=n && (n<=0x0CB9 
>    ||  0x0CDE==n 
>    ||  0x0CE0<=n && (n<=0x0CE1 
>    ||  0x0D05<=n && (n<=0x0D0C 
>    ||  0x0D0E<=n && (n<=0x0D10 
>    ||  0x0D12<=n && (n<=0x0D28 
>    ||  0x0D2A<=n && (n<=0x0D39 
>    ||  0x0D60<=n && (n<=0x0D61 
>    ||  0x0E01<=n && (n<=0x0E2E 
>    ||  0x0E30==n 
>    ||  0x0E32<=n && (n<=0x0E33 
>    ||  0x0E40<=n && (n<=0x0E45 
>    ||  0x0E81<=n && (n<=0x0E82 
>    ||  0x0E84==n 
>    ||  0x0E87<=n && (n<=0x0E88 
>    ||  0x0E8A==n 
>    ||  0x0E8D==n 
>    ||  0x0E94<=n && (n<=0x0E97 
>    ||  0x0E99<=n && (n<=0x0E9F 
>    ||  0x0EA1<=n && (n<=0x0EA3 
>    ||  0x0EA5==n 
>    ||  0x0EA7==n 
>    ||  0x0EAA<=n && (n<=0x0EAB 
>    ||  0x0EAD<=n && (n<=0x0EAE 
>    ||  0x0EB0==n 
>    ||  0x0EB2<=n && (n<=0x0EB3 
>    ||  0x0EBD==n 
>    ||  0x0EC0<=n && (n<=0x0EC4 
>    ||  0x0F40<=n && (n<=0x0F47 
>    ||  0x0F49<=n && (n<=0x0F69 
>    ||  0x10A0<=n && (n<=0x10C5 
>    ||  0x10D0<=n && (n<=0x10F6 
>    ||  0x1100==n 
>    ||  0x1102<=n && (n<=0x1103 
>    ||  0x1105<=n && (n<=0x1107 
>    ||  0x1109==n 
>    ||  0x110B<=n && (n<=0x110C 
>    ||  0x110E<=n && (n<=0x1112 
>    ||  0x113C==n 
>    ||  0x113E==n 
>    ||  0x1140==n 
>    ||  0x114C==n 
>    ||  0x114E==n 
>    ||  0x1150==n 
>    ||  0x1154<=n && (n<=0x1155 
>    ||  0x1159==n 
>    ||  0x115F<=n && (n<=0x1161 
>    ||  0x1163==n 
>    ||  0x1165==n 
>    ||  0x1167==n 
>    ||  0x1169==n 
>    ||  0x116D<=n && (n<=0x116E 
>    ||  0x1172<=n && (n<=0x1173 
>    ||  0x1175==n 
>    ||  0x119E==n 
>    ||  0x11A8==n 
>    ||  0x11AB==n 
>    ||  0x11AE<=n && (n<=0x11AF 
>    ||  0x11B7<=n && (n<=0x11B8 
>    ||  0x11BA==n 
>    ||  0x11BC<=n && (n<=0x11C2 
>    ||  0x11EB==n 
>    ||  0x11F0==n 
>    ||  0x11F9==n 
>    ||  0x1E00<=n && (n<=0x1E9B 
>    ||  0x1EA0<=n && (n<=0x1EF9 
>    ||  0x1F00<=n && (n<=0x1F15 
>    ||  0x1F18<=n && (n<=0x1F1D 
>    ||  0x1F20<=n && (n<=0x1F45 
>    ||  0x1F48<=n && (n<=0x1F4D 
>    ||  0x1F50<=n && (n<=0x1F57 
>    ||  0x1F59==n 
>    ||  0x1F5B==n 
>    ||  0x1F5D==n 
>    ||  0x1F5F<=n && (n<=0x1F7D 
>    ||  0x1F80<=n && (n<=0x1FB4 
>    ||  0x1FB6<=n && (n<=0x1FBC 
>    ||  0x1FBE==n 
>    ||  0x1FC2<=n && (n<=0x1FC4 
>    ||  0x1FC6<=n && (n<=0x1FCC 
>    ||  0x1FD0<=n && (n<=0x1FD3 
>    ||  0x1FD6<=n && (n<=0x1FDB 
>    ||  0x1FE0<=n && (n<=0x1FEC 
>    ||  0x1FF2<=n && (n<=0x1FF4 
>    ||  0x1FF6<=n && (n<=0x1FFC 
>    ||  0x2126==n 
>    ||  0x212A<=n && (n<=0x212B 
>    ||  0x212E==n 
>    ||  0x2180<=n && (n<=0x2182 
>    ||  0x3041<=n && (n<=0x3094 
>    ||  0x30A1<=n && (n<=0x30FA 
>    ||  0x3105<=n && (n<=0x312C 
>    ||  0xAC00<=n && n<=0xD7A3)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
>     where 
>       n = ord c

]]></code></production>
<production nr="213">
<code><![CDATA[

> isIdeographic c
>  =  
>        0x4E00<=n && (n<=0x9FA5 
>    ||  0x3007==n  
>    ||  0x3021<=n &&  n<=0x3029)
>     where 
>       n = ord c

]]></code></production>
<production nr="214">
<code><![CDATA[

> isCombiningChar c
>  =     0x0300<=n && (n<=0x0345 
>    ||  0x0360<=n && (n<=0x0361 
>    ||  0x0483<=n && (n<=0x0486 
>    ||  0x0591<=n && (n<=0x05A1 
>    ||  0x05A3<=n && (n<=0x05B9 
>    ||  0x05BB<=n && (n<=0x05BD 
>    ||  0x05BF==n   
>    ||  0x05C1<=n && (n<=0x05C2 
>    ||  0x05C4==n   
>    ||  0x064B<=n && (n<=0x0652 
>    ||  0x0670==n   
>    ||  0x06D6<=n && (n<=0x06DC 
>    ||  0x06DD<=n && (n<=0x06DF 
>    ||  0x06E0<=n && (n<=0x06E 
>    ||  0x06E7<=n && (n<=0x06E8 
>    ||  0x06EA<=n && (n<=0x06ED 
>    ||  0x0901<=n && (n<=0x0903 
>    ||  0x093C==n   
>    ||  0x093E<=n && (n<=0x094C 
>    ||  0x094D==n   
>    ||  0x0951<=n && (n<=0x0954 
>    ||  0x0962<=n && (n<=0x0963 
>    ||  0x0981<=n && (n<=0x0983 
>    ||  0x09BC==n   
>    ||  0x09BE==n   
>    ||  0x09BF==n   
>    ||  0x09C0<=n && (n<=0x09C4 
>    ||  0x09C7<=n && (n<=0x09C8 
>    ||  0x09CB<=n && (n<=0x09CD 
>    ||  0x09D7==n   
>    ||  0x09E2<=n && (n<=0x09E3 
>    ||  0x0A02==n   
>    ||  0x0A3C==n   
>    ||  0x0A3E==n   
>    ||  0x0A3F==n   
>    ||  0x0A40<=n && (n<=0x0A42 
>    ||  0x0A47<=n && (n<=0x0A48 
>    ||  0x0A4B<=n && (n<=0x0A4D 
>    ||  0x0A70<=n && (n<=0x0A71 
>    ||  0x0A81<=n && (n<=0x0A83 
>    ||  0x0ABC==n   
>    ||  0x0ABE<=n && (n<=0x0AC5 
>    ||  0x0AC7<=n && (n<=0x0AC9 
>    ||  0x0ACB<=n && (n<=0x0ACD 
>    ||  0x0B01<=n && (n<=0x0B03 
>    ||  0x0B3C==n   
>    ||  0x0B3E<=n && (n<=0x0B43 
>    ||  0x0B47<=n && (n<=0x0B48 
>    ||  0x0B4B<=n && (n<=0x0B4D 
>    ||  0x0B56<=n && (n<=0x0B57 
>    ||  0x0B82<=n && (n<=0x0B83 
>    ||  0x0BBE<=n && (n<=0x0BC2 
>    ||  0x0BC6<=n && (n<=0x0BC8 
>    ||  0x0BCA<=n && (n<=0x0BCD 
>    ||  0x0BD7==n   
>    ||  0x0C01<=n && (n<=0x0C03 
>    ||  0x0C3E<=n && (n<=0x0C44 
>    ||  0x0C46<=n && (n<=0x0C48 
>    ||  0x0C4A<=n && (n<=0x0C4D 
>    ||  0x0C55<=n && (n<=0x0C56 
>    ||  0x0C82<=n && (n<=0x0C83 
>    ||  0x0CBE<=n && (n<=0x0CC4 
>    ||  0x0CC6<=n && (n<=0x0CC8 
>    ||  0x0CCA<=n && (n<=0x0CCD 
>    ||  0x0CD5<=n && (n<=0x0CD6 
>    ||  0x0D02<=n && (n<=0x0D03 
>    ||  0x0D3E<=n && (n<=0x0D43 
>    ||  0x0D46<=n && (n<=0x0D48 
>    ||  0x0D4A<=n && (n<=0x0D4D 
>    ||  0x0D57==n   
>    ||  0x0E31==n   
>    ||  0x0E34<=n && (n<=0x0E3A 
>    ||  0x0E47<=n && (n<=0x0E4E 
>    ||  0x0EB1==n   
>    ||  0x0EB4<=n && (n<=0x0EB9 
>    ||  0x0EBB<=n && (n<=0x0EBC 
>    ||  0x0EC8<=n && (n<=0x0ECD 
>    ||  0x0F18<=n && (n<=0x0F19 
>    ||  0x0F35==n   
>    ||  0x0F37==n   
>    ||  0x0F39==n   
>    ||  0x0F3E==n   
>    ||  0x0F3F==n   
>    ||  0x0F71<=n && (n<=0x0F84 
>    ||  0x0F86<=n && (n<=0x0F8B 
>    ||  0x0F90<=n && (n<=0x0F95 
>    ||  0x0F97==n   
>    ||  0x0F99<=n && (n<=0x0FAD 
>    ||  0x0FB1<=n && (n<=0x0FB7 
>    ||  0x0FB9==n   
>    ||  0x20D0<=n && (n<=0x20DC 
>    ||  0x20E1==n   
>    ||  0x302A<=n && (n<=0x302F 
>    ||  0x3099==n   
>    ||  0x309A==n))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))  
>     where 
>       n = ord c

]]></code></production>
<production nr="216">
<code><![CDATA[

> isTDigit c
>  =     0x0030<=n && (n<=0x0039 
>    ||  0x0660<=n && (n<=0x0669 
>    ||  0x06F0<=n && (n<=0x06F9 
>    ||  0x0966<=n && (n<=0x096F 
>    ||  0x09E6<=n && (n<=0x09EF 
>    ||  0x0A66<=n && (n<=0x0A6F 
>    ||  0x0AE6<=n && (n<=0x0AEF 
>    ||  0x0B66<=n && (n<=0x0B6F 
>    ||  0x0BE7<=n && (n<=0x0BEF 
>    ||  0x0C66<=n && (n<=0x0C6F 
>    ||  0x0CE6<=n && (n<=0x0CEF 
>    ||  0x0D66<=n && (n<=0x0D6F 
>    ||  0x0E50<=n && (n<=0x0E59 
>    ||  0x0ED0<=n && (n<=0x0ED9 
>    ||  0x0F20<=n && n<=0x0F29))))))))))))))
>     where 
>       n = ord c

]]></code></production>

<production nr="216">
<code><![CDATA[

> isExtender c
>  = 
>        0x00B7==n  
>    ||  0x02D0==n  
>    ||  0x02D1==n  
>    ||  0x0387==n  
>    ||  0x0640==n  
>    ||  0x0E46==n  
>    ||  0x0EC6==n  
>    ||  0x3005==n 
>    ||  0x3031<=n && (n<=0x3035 
>    ||  0x309D<=n && (n<=0x309E 
>    ||  0x30FC<=n && (n<=0x30FE)))
>     where 
>       n = ord c

]]></code></production>
</section>
