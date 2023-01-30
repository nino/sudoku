let css_provider_from_data data =
  let provider = GObj.css_provider () in
  provider#load_from_data data;
  provider

let set_css widget css =
  widget#misc#style_context#add_provider (css_provider_from_data css) 0
