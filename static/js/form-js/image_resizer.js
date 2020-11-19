

function imageResizer(image, div, image_width, image_height){
    if(!image || image_width <= 0.0 || image_height <= 0.0) return true;
    
    var image_ratio = image_height/image_width;
    var div_ratio = div.height()/div.width();
    if(image_ratio < div_ratio) {
        image.width(div.width());
        image.height(div.width()*image_ratio);
    }
    else {
        image.height(div.height());
        image.width(div.height()/image_ratio);
    }
    return false;
};
